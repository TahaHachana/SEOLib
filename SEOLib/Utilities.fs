namespace SEOLib

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.RegularExpressions
open Types
open StopWords

module internal Utilities =

    [<AutoOpenAttribute>]
    module Html =

        /// Decodes HTML encoded characters.
        let inline decodeHtml html = WebUtility.HtmlDecode html

        let inline checkEmptyString str = match str with "" -> None | _ -> Some str
        
        let checkEmptyString' = decodeHtml >> checkEmptyString

        ///Compiles a pattern into a Regex object.
        let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

        /// Returns the trimmed value of a group in a match object.
        let inline groupValue (idx : int) (regex : Regex) str =
            regex.Match(str).Groups.[idx].Value.Trim()

        let inline groupValue' (idx : int) (matchObj : Match) =
            matchObj.Groups.[idx].Value.Trim()

        let inline fstGroupValue regex str = groupValue 0 regex str
        let inline sndGroupValue regex str = groupValue 1 regex str
        let inline trdGroupValue regex str = groupValue 2 regex str
        let inline frtGroupValue regex str = groupValue 3 regex str

        let inline fstGroupValue' matchObj = groupValue' 0 matchObj
        let inline sndGroupValue' matchObj = groupValue' 1 matchObj
        let inline trdGroupValue' matchObj = groupValue' 2 matchObj
        let inline frtGroupValue' matchObj = groupValue' 3 matchObj

        let attributePattern   = "(?i)(\w+)=(\"|')(.+?)(\"|')"
        let contentAttrPattern = "(?i)content"
        let headingPattern     = "(?is)<h([1-6])>(.+?)</h[1-6]>"
        let metaDescPattern    = "(?i)name=(\"|')description(\"|')"
        let metaKeysPattern    = "(?i)name=(\"|')keywords(\"|')"
        let metaTagPattern     = "(?i)<meta .+?>"
        let tagPattern         = "<.+?>"
        let titlePattern       = "(?is)<title>(.+?)</title>"

        let attributeRegex   = compileRegex attributePattern
        let contentAttrRegex = compileRegex contentAttrPattern
        let headingRegex     = compileRegex headingPattern
        let metaDescRegex    = compileRegex metaDescPattern
        let metaKeysRegex    = compileRegex metaKeysPattern
        let metaTagRegex     = compileRegex metaTagPattern
        let tagRegex         = compileRegex tagPattern
        let titleRegex       = compileRegex titlePattern

        let inline makeHtmlAttribute key value : HtmlAttribute =
            {
                Key   = key
                Value = value
            }

        let inline tagAttributes tag =
            attributeRegex.Matches tag
            |> Seq.cast<Match>
            |> Seq.toArray
            |> Array.map (fun x ->
                makeHtmlAttribute (sndGroupValue' x) (frtGroupValue' x))

        let inline metaTag metaTags (regex : Regex) (regex' : Regex) =
            metaTags
            |> Array.tryFind regex.IsMatch
            |> function
                | None -> None
                | Some metaDesc ->
                    tagAttributes metaDesc
                    |> Array.tryFind (fun x -> regex'.IsMatch x.Key)
                    |> function
                        | None -> None
                        | Some attr ->
                            attr.Value
                            |> decodeHtml
                            |> Some

        let stripTags html = tagRegex.Replace(html, "").Trim()

        let makeHeading rank value =
            let value' = stripTags value |> decodeHtml
            match rank with
                | "1" -> H1 value'
                | "2" -> H2 value'
                | "3" -> H3 value'
                | "4" -> H4 value'
                | "5" -> H5 value'
                | _   -> H6 value'

    [<AutoOpenAttribute>]
    module Http =
        
        // IE9 user-agent string.
        let ie9UserAgent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"
    
        /// Initializes a HttpClient instance and adds a user-agent request header. 
        let inline httpClient () =
            let client = new HttpClient()
            client.DefaultRequestHeaders.Add("User-Agent", ie9UserAgent)
            client.MaxResponseContentBufferSize <- int64 1073741824
            client

        /// Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
        let inline awaitHttpResponse (client : HttpClient) (requestUri : Uri) = client.GetAsync requestUri |> Async.AwaitTask

        /// Returns an asynchronous computation that will wait for the task of reading the content of a HTTP response message as a string.
        let inline awaitReadAsString (httpContent : HttpContent) = httpContent.ReadAsStringAsync() |> Async.AwaitTask

        /// Initializes a Header instance.
        let inline constructHeader key (value : string seq) = {Key = key; Value = Seq.toList value}
        
        let inline httpHeaders (responseHeaders : HttpResponseHeaders) (contentHeaders : HttpContentHeaders) =
            [
                for x in responseHeaders do
                    yield constructHeader x.Key x.Value
                for x in contentHeaders do
                    yield constructHeader x.Key x.Value
            ]

        let (|HTML|NotHTML|) = function
            | "text/html" -> HTML
            | _           -> NotHTML

        /// Reads HttpContent as a string if its content type is HTML.        
        let readHttpContent statusCode mediaType httpContent =
            async { 
                try
                    match statusCode with
                        | System.Net.HttpStatusCode.OK ->
                            match mediaType with
                                | HTML  ->
                                    let! html = awaitReadAsString httpContent
                                    return Some html
                                | _ -> return None
                        | _ -> return None
                with _ -> return None
            }

        let inline strByteSize (strOption: string option) =
            match strOption with
                | None -> None
                | Some str ->
                    Encoding.UTF8.GetByteCount str
                    |> float
                    |> (fun x -> x / 1024.)
                    |> Math.Round
                    |> int
                    |> Some

        let makeHttpData requestUri statusCode headers mediaType htmlOption elapsedTime =
            let sizeOption = strByteSize htmlOption
            {
                RequestUri  = requestUri
                StatusCode  = statusCode
                Headers     = headers
                MediaType   = mediaType
                Size        = sizeOption 
                Html        = htmlOption
                ElapsedTime = elapsedTime
            }

    [<AutoOpenAttribute>]
    module Keywords =

        let stopWords = Array.toList englishStopWords

        let rec removeStopWords stopWords str =
            match stopWords with
            | h :: t ->
                let regex = compileRegex <| "\\b" + h + "\\b" 
                let str'  = regex.Replace(str, "*")
                removeStopWords t str'
            | [] -> str

        let inline round (x : float) = Math.Round(x, 2)

        let computeDensity count count' length =
            float count / count' * 100. * length
            |> round

        let makeKeyword wordsCount combination occurrence density =
            {
                WordsCount  = wordsCount
                Combination = combination
                Occurrence  = occurrence
                Density     = density
            }

        let keywordData (matchCollection : Match seq) str count length =
            let wordsCount = int length
            matchCollection
            |> Seq.map (fun x -> x.Value)
            |> Seq.distinct
            |> Seq.map (fun x -> x, Regex("\\b" + x + "\\b").Matches(str).Count)
            |> Seq.map (fun (x, y) -> x, y, computeDensity y count length)
            |> Seq.sortBy (fun (_, x, _) -> x)
            |> Seq.toArray
            |> Array.rev
            |> Array.map (fun (x, y, z) -> makeKeyword wordsCount x y z)

        let regexMatches (regex : Regex) str = regex.Matches str |> Seq.cast<Match>

        let  keywordData' regex str count length =
            let matchCollection = regexMatches regex str
            keywordData matchCollection str count length

        let commentJSCssPattern = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
        let oneWordPattern      = @"\b\w+\b"
        let twoWordsPattern     = @"\b\w+\b \b\w+\b"
        let threeWordsPattern   = @"\b\w+\b \b\w+\b \b\w+\b"

        let commentJSCssRegex  = compileRegex commentJSCssPattern
        let oneKeywordRegex    = compileRegex oneWordPattern
        let twoKeywordsRegex   = compileRegex twoWordsPattern
        let threeKeywordsRegex = compileRegex threeWordsPattern

        let twoKeywordsData   str count = keywordData' twoKeywordsRegex   str count 2.
        let threeKeywordsData str count = keywordData' threeKeywordsRegex str count 3.

        let matchCount (matchCollection : Match seq) = matchCollection |> Seq.length |> float

        let cleanHtml html =
            let html' = commentJSCssRegex.Replace(html, "*")
            let html'' = tagRegex.Replace(html', "*") |> decodeHtml
            html''.ToLower()

    [<AutoOpenAttribute>]
    module Links =

        let absoluteUriPattern = "(?i)^https?://[^\"]*"
        let aTagPattern = "(?is)(<a .+?>)(.+?)</a>"
        let baseTagPattern = "(?is)<base.+?>"
        let commentPattern = "(?s)<!--.*?--\s*>"
        let hrefPattern = "(?i) href\\s*=\\s*(\"|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"'\#]+)(\"|'|\#)"
        let relAttributePattern = "rel=(\"|')(.+?)(\"|')"
        let noFollowPattern = "(?i)nofollow"
        let inlineJsCssPattern = "<script.*?</script>|<style.*?</style>"

        let absoluteUriRegex = compileRegex absoluteUriPattern
        let aTagRegex = compileRegex aTagPattern
        let baseTagRegex = compileRegex baseTagPattern
        let commentRegex = compileRegex commentPattern
        let hrefRegex = compileRegex hrefPattern    
        let relAttributeRegex = compileRegex relAttributePattern
        let noFollowRegex = compileRegex noFollowPattern
        let inlineJsCssRegex = compileRegex inlineJsCssPattern

        /// Returns the href attribute value of the <base> tag in a HTML document.
        let baseUri html requestUri =
            let baseTagMatch = baseTagRegex.Match html
            baseTagMatch.Success |> function
                | false -> requestUri
                | true  ->
                    let hrefMatch = hrefRegex.Match baseTagMatch.Value
                    match hrefMatch.Success with
                        | false -> requestUri
                        | true  ->
                            let group2 = trdGroupValue' hrefMatch
                            Uri.TryCreate(group2, UriKind.Absolute) |> function
                                | false, _   -> requestUri
                                | true , uri -> uri
            |> Some

        let relAttributes aTag =
            trdGroupValue relAttributeRegex aTag
            |> (fun x -> x.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
            |> List.ofArray
            |> List.map (fun x -> x.Trim())

        let stripInlineJsCss str = inlineJsCssRegex.Replace(str, "")

        let linkAnchor matchObj =
            trdGroupValue' matchObj
            |> stripInlineJsCss
            |> stripTags
            |> decodeHtml

        /// Scrapes href attributes from <a> tags in an HTML string.
        let collectHrefs html =
            aTagRegex.Matches html
            |> Seq.cast<Match>
            |> Seq.toList
            |> List.map (fun x -> sndGroupValue' x, linkAnchor x)
            |> List.map (fun (openTag, anchor) -> openTag, relAttributes openTag, anchor)
            |> List.map (fun (openTag, relAttrs, anchor) ->
                let href = trdGroupValue hrefRegex openTag
                let follow =
                    relAttrs
                    |> List.tryFind (fun x -> noFollowRegex.IsMatch x)
                    |> function Some _ -> NoFollow | None -> DoFollow
                href, anchor, follow)

        let partitionLinks links = List.partition (fun (href, _, _) -> absoluteUriRegex.IsMatch href) links

        let formatRelativeUris relativeHrefs uri baseUri =
            [
                let uri' =
                    match baseUri with
                        | None   -> uri
                        | Some x -> x
                for (href : string, anchor, follow) in relativeHrefs do
                    let isUri, result = Uri.TryCreate(uri', href)
                    match isUri with
                        | true  -> yield result, anchor, follow
                        | false -> ()
            ]

        let removeComments html = commentRegex.Replace(html, "*")

        let makeLink (uri : Uri) anchor linkType follow =
            {
                URL    = uri.ToString()
                Anchor = anchor
                Type   = linkType
                Follow = follow
            }

        let makeLink' (requestUri : Uri) (uriData : (Uri * string * Follow) list) =
            let host = requestUri.Host
            uriData |> List.map (fun (uri, anchor, follow) ->
                let isInternal = uri.Host = host
                match isInternal with
                    | false -> makeLink uri anchor External follow
                    | true  -> makeLink uri anchor Internal follow)

    [<AutoOpenAttribute>]
    module Violations =

        let altEmptyHeading            = "The ALT attribute of the <img> tag is empty."
        let altMissingHeading          = "The ALT attribute of the <img> tag is missing."
        let descriptionEmptyHeading    = "The meta description is empty."
        let descriptionLongHeading     = "The meta description is too long."
        let descriptionMissingHeading  = "The meta description is missing."
        let descriptionMultipleHeading = "The page contains multiple meta description tags."
        let descriptionShortHeading    = "The meta description is too short."
        let h1EmptyHeading             = "The <h1> tag is empty."
        let h1MissingHeading           = "The <h1> tag is missing."
        let h1MultipleHeading          = "The page contains multiple <h1> tags."
        let largeInlineCssHeading      = "The page contains a large number of CSS definitions."
        let largeInlineScriptHeading   = "The page contains a large amount of script code."
        let queryParameterCountHeading = "The URL contains too many query string arguments."
        let titleEqDescriptionHeading  = "The title and description are identical."
        let titleEmptyHeading          = "The title is empty."
        let titleLongHeading           = "The title is too long."
        let titleMissingHeading        = "The title is missing."
        let titleShortHeading          = "The title is too short."
        let manyLinksHeading           = "The page contains too many hyperlinks."
        let refreshRedirectHeading     = "The page uses a refresh definition instead of using redirection."

        let altEmptyDescription                  = "The ALT attribute is used to provide relevant information about the image to the search engines."
        let altMissingDescription                = altEmptyDescription
        let descriptionEmptyDescription          = "The <meta name=\"description\" /> tag does not have a content. The description meta tag is used to provide a short description of the page content."
        let descriptionLongDescription length    = sprintf "The content within the <meta name=\"description\"> tag is too long (%d characters). The description must not exceed 150 characters." length
        let descriptionMissingDescription        = "The page markup does not contain the <meta name=\"description\" /> tag."
        let descriptionMultipleDescription count = sprintf "The page markup contains %d <meta name=\"description\" /> tag." count
        let descriptionShortDescription length   = sprintf "The content in the <meta name=\"description\"> tag is too short (%d characters). The description must be at least 25 characters long." length
        let h1EmptyDescription                   = "An <h1> tag is an indicator of what the page content is about."
        let h1MissingDescription                 = h1EmptyDescription
        let h1MultipleDescription                = "An <h1> tag is an indicator of what the page content is about. Search engines diminish the value of a page that contains multiple <h1> tags."
        let largeInlineCssDescription length     = sprintf "The page contains a large block of embedded Cascading Style Sheet (CSS) code (%d characters). Search engines will ignore CSS code, but large quantities of CSS code that precede the actual text content of the page will force the text content further down in the HTML." length
        let largeInlineScriptDescription length  = sprintf "The page contains a large block of embedded script code (%d characters). Search engines will ignore script code, but large quantities of script will force the actual text content of the page further down in the HTML." length
        let queryParameterCountDescription count = sprintf "The URL contains %d query string parameters. Most search engines will analyze up to four URL parameters and ignore any additional parameters." count
        let titleEqDescriptionDescription        = "The <title> and the <meta name=\"description\"> tags of the page contain identical text."
        let titleEmptyDescription                = "The <title> tag of the page is empty. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
        let titleLongDescription length          = sprintf "The text in the <title> tag is too long (%d characters). The title must not exceed 65 characters, including spaces. Most search engines will truncate the text in the <title> tag after a fixed number of characters." length
        let titleMissingDescription              = "The page markup does not contain the <title> tag. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
        let titleShortDescription length         = sprintf "The text in the <title> tag is too short (%d characters). The title must contain at least five (5) characters. A title that is too short can affect how the content of the page is indexed, ranked, and presented by search engines." length
        let manyLinksDescription count           = sprintf "The page contains %d links. Too many links in the same page may affect the indexing process that is used by search engine crawlers. This may result in poor search rankings for the page or in the search engine ignoring the page." count
        let refreshRedirectDescription           = "The page uses a meta refresh tag to implement redirection. Using meta refresh syntax is discouraged by the W3C, because unexpected refresh may confuse users and may impair the \"back\" button functionality in some Web browsers."

        let altEmptyRecommendation            = "Add content to the \"alt\" attribute."
        let altMissingRecommendation          = "Add an \"alt\" attribute to the <img> tag."
        let descriptionEmptyRecommendation    = "Modify the <meta name=\"description\" /> tag in the <head> section of the page to contain the actual page content description."
        let descriptionLongRecommendation     = "The description must be between 25 and 150 characters long, human readable, actionable, and rich in keywords."
        let descriptionMissingRecommendation  = "Add <meta name=\"description\" content=\"description_text\"> to the <head> section of the page."
        let descriptionMultipleRecommendation = "Remove extra <meta name='description' /> tags from the page."
        let descriptionShortRecommendation    = descriptionLongRecommendation
        let h1EmptyRecommendation             = "Add a keyword rich heading inside the <h1> tag."
        let h1MissingRecommendation           = "Add an <h1> tag to the <body> section of the page."
        let h1MultipleRecommendation          = "Use only one <h1> tag per page."
        let largeInlineCssRecommendation      = "Move large blocks of CSS code to externally linked CSS files."
        let largeInlineScriptRecommendation   = "Move all inline script code to externally linked script files."
        let queryParameterCountRecommendation = "Consider reducing the number of query string parameters, or use a URL Rewrite Module to simplify the URL structure."
        let titleEqDescriptionRecommendation  = "Don't reuse the title content in the meta description of the page."
        let titleEmptyRecommendation          = "Add the title text inside the <title> tag of the page."
        let titleLongRecommendation           = "The title must be between 5 and 65 characters long."
        let titleMissingRecommendation        = "Add the <title> tag inside of the <head> section of the page."
        let titleShortRecommendation          = titleLongRecommendation
        let manyLinksRecommendation           = "Reduce the number of links on the page to less than 250."
        let refreshRedirectRecommendation     = "Use HTTP Redirection instead of using HTTP Refresh headers or meta refresh tags."


        let makeAltEmptyViolation indexOption =
            {
                Category       = SEO
                Code           = AltEmpty
                Description    = altEmptyDescription
                Heading        = altEmptyHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = altEmptyRecommendation
            }

        let makeAltMissingViolation indexOption =
            {
                Category       = SEO
                Code           = AltMissing
                Description    = altMissingDescription
                Heading        = altMissingHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = altMissingRecommendation
            }

        let makeDescriptionEmptyViolation indexOption =
            {
                Category       = SEO
                Code           = DescriptionEmpty
                Description    = descriptionEmptyDescription
                Heading        = descriptionEmptyHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = descriptionEmptyRecommendation
            }

        let makeDescriptionLongViolation length indexOption =
            {
                Category       = SEO
                Code           = DescriptionLong
                Description    = descriptionLongDescription length
                Heading        = descriptionLongHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = descriptionLongRecommendation
            }

        let makeDescriptionMissingViolation indexOption =
            {
                Category       = SEO
                Code           = DescriptionMissing
                Description    = descriptionMissingDescription
                Heading        = descriptionMissingHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = descriptionMissingRecommendation
            }

        let makeDescriptionMultipleViolation count indexOption =
            {
                Category       = Standards
                Code           = DescriptionMultiple
                Description    = descriptionMultipleDescription count
                Heading        = descriptionMultipleHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = descriptionMultipleRecommendation
            }

        let makeDescriptionShortViolation length indexOption =
            {
                Category       = SEO
                Code           = DescriptionShort
                Description    = descriptionShortDescription length
                Heading        = descriptionShortHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = descriptionShortRecommendation
            }

        let makeH1EmptyViolation indexOption =
            {
                Category       = SEO
                Code           = H1Empty
                Description    = h1EmptyDescription
                Heading        = h1EmptyHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = h1EmptyRecommendation
            }

        let makeH1MissingViolation indexOption =
            {
                Category       = SEO
                Code           = H1Missing
                Description    = h1MissingDescription
                Heading        = h1MissingHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = h1MissingRecommendation
            }

        let makeH1MultipleViolation indexOption =
            {
                Category       = Standards
                Code           = H1Multiple
                Description    = h1MultipleDescription
                Heading        = h1MultipleHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = h1MultipleRecommendation
            }

        let makeLargeInlineCssViolation length indexOption =
            {
                Category       = Performance
                Code           = LargeInlineCss
                Description    = largeInlineCssDescription length
                Heading        = largeInlineCssHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = largeInlineCssRecommendation
            }

        let makeLargeInlineScriptViolation length indexOption =
            {
                Category       = Performance
                Code           = LargeInlineScript
                Description    = largeInlineScriptDescription length
                Heading        = largeInlineScriptHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = largeInlineScriptRecommendation
            }

        let makeQueryParameterCountViolation count =
            {
                Category       = SEO
                Code           = QueryParameterCount
                Description    = queryParameterCountDescription count
                Heading        = queryParameterCountHeading
                Index          = None
                Level          = Warning
                Recommendation = queryParameterCountRecommendation
            }

        let makeTitleEqDescriptionViolation indexOption =
            {
                Category       = SEO
                Code           = TitleEqDescription
                Description    = titleEqDescriptionDescription
                Heading        = titleEqDescriptionHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = titleEqDescriptionRecommendation
            }

        let makeTitleEmptyViolation indexOption =
            {
                Category       = SEO
                Code           = TitleEmpty
                Description    = titleEmptyDescription
                Heading        = titleEmptyHeading
                Index          = indexOption
                Level          = Error
                Recommendation = titleEmptyRecommendation
            }

        let makeTitleLongViolation length indexOption =
            {
                Category       = SEO
                Code           = TitleLong
                Description    = titleLongDescription length
                Heading        = titleLongHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = titleLongRecommendation
            }

        let makeTitleMissingViolation indexOption =
            {
                Category       = SEO
                Code           = TitleMissing
                Description    = titleMissingDescription
                Heading        = titleMissingHeading
                Index          = indexOption
                Level          = Error
                Recommendation = titleMissingRecommendation
            }

        let makeTitleShortViolation length indexOption =
            {
                Category       = SEO
                Code           = TitleShort
                Description    = titleShortDescription length
                Heading        = titleShortHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = titleShortRecommendation
            }

        let makeTooManyLinksViolation count =
            {
                Category       = SEO
                Code           = TooManyLinks
                Description    = manyLinksDescription count
                Heading        = manyLinksHeading
                Index          = None
                Level          = Error
                Recommendation = manyLinksRecommendation
            }

        let makeRefreshRedirectViolation indexOption =
            {
                Category       = Standards
                Code           = RefreshToRedirect
                Description    = refreshRedirectDescription
                Heading        = refreshRedirectHeading
                Index          = indexOption
                Level          = Warning
                Recommendation = refreshRedirectRecommendation
            }


        let altAttributePattern = "(?i)alt=(\"|')(.+?)(\"|')"
        let h1Pattern           = "(?is)<h1>(.+?)</h1>"
        let imgTagPattern       = "(?is)<img.+?>"
        let inlineCssPattern    = "(?is)<style .*?</style>"
        let inlineScriptPattern = "(?is)<script .*?</script>"
        let queryParamPattern   = "(\?|&)[^=]+"
        let metaRefreshPattern  = "(?i)name=(\"|')refresh(\"|')"

        let altAttributeRegex = compileRegex altAttributePattern
        let h1Regex           = compileRegex h1Pattern
        let imgTagRegex       = compileRegex imgTagPattern
        let inlineCssRegex    = compileRegex inlineCssPattern
        let inlineScriptRegex = compileRegex inlineScriptPattern
        let queryParamRegex   = compileRegex queryParamPattern
        let metaRefreshRegex  = compileRegex metaRefreshPattern

        let altAttribute (imgTagMatch : Match) =
            let value = imgTagMatch.Value
            let index = imgTagMatch.Index
            let altMatch = altAttributeRegex.Match value
            altMatch.Success |> function
                | true ->
                    let altAttr = trdGroupValue' altMatch |> Some
                    value, index, altAttr
                | false -> value, index, None

        let metaTags html =
            metaTagRegex.Matches html
            |> Seq.cast<Match>
            |> Seq.toArray
            |> Array.map (fun x -> x.Value, x.Index)

        let metaDescTags html =
            metaTags html
            |> Array.filter (fun (x, _) -> metaDescRegex.IsMatch x)

        let metaRefreshTag html =
            metaTags html
            |> Array.tryFind (fun (x, _) -> metaRefreshRegex.IsMatch x)

        let h1Tags html = h1Regex.Matches html |> Seq.cast<Match>

        let f regex html g =
            regexMatches regex html
            |> Seq.toArray
            |> Array.map (fun x -> x.Value, x.Index)
            |> Array.filter (fun (value, _) -> value.Length > 1024)
            |> Array.map (fun (value, index) ->
                let index' = Some index
                let length = value.Length
                g length index')

        let title html =
            let titleMatch = titleRegex.Match html
            titleMatch.Success |> function
                | false -> None
                | true  ->
                    (sndGroupValue' titleMatch |> checkEmptyString', titleMatch.Index)
                    |> Some








//    let htmlTagPattern         = "(?i)<[^>]*>"
//    let metaDescriptionPattern = "(?i)name=('|\")description('|\")"
//    let metaKeywordsPattern    = "(?i)name=('|\")keywords('|\")"
//
//    let htmlTagRegex         = compileRegex htmlTagPattern
//    let metaDescriptionRegex = compileRegex metaDescriptionPattern
//    let metaKeywordsRegex    = compileRegex metaKeywordsPattern

//
//open System
//open System.IO
//open System.Net
//open System.Net.Http
//open System.Text.RegularExpressions
//open Types
//
//module internal Utilities =
//
//    /// Compiles a regex.
//    let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)
//
//    /// Rounds the third element of a tuple.
//    let roundThird (x, y, z : float) = x, y, Math.Round(z, 2)
//
//    /// Returns the value of a Nullable<int> and zero if doesn't have one.
//    let hasValue (nullable : Nullable<int64>) =
//        nullable.HasValue |> function
//            | false -> 0
//            | true  -> nullable.Value |> int
//
//    /// Optionally returns the value of a Nullable<int>.
//    let hasValue' (nullable : Nullable<int64>) =
//        nullable.HasValue |> function
//            | false -> None
//            | true  -> nullable.Value |> int |> Some
//
//    let testNull = function null -> 0 | (x : string) -> int x
//
//    // Regex patterns.
//    let absoluteUriPattern   = "(?i)^https?://[^\"]*"
//    let altAttributePattern  = "(?i)alt=(\"|')(.+?)(\"|')"
//    let aTagPattern          = "(?i)<a.+?>"
//    let commentJSCssPattern  = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
//    let crawlDelayPattern    = "(?i)^crawl-delay:(.+)"
//    let googleBotPattern     = "(?i)googlebot"
//    let h1TagPattern         = "(?is)<h1>(.+?)</h1>"
//    let hrefPattern          = "(?i)href\\s*=\\s*(\"|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"'\#]+)(\"|'|\#)"
//    let htmlContentPattern   = "(?i)html"
//    let htmlTagPattern       = "(?i)<[^>]*>"
//    let imgTagPattern        = "(?is)<img.+?>"
//    let inlineCssPattern     = "(?is)<style.*?</style>"
//    let inlineJsPattern      = "(?is)<script.*?</script>"
//    let metaContentPattern   = "content=(\"|')(.+)(\"|')"
//    let metaDescContPattern  = "(?is)content=(\"|')(.+?)(\"|')"
//    let metaDescPattern      = "(?i)name=(\"|')description(\"|')"
//    let metaRefreshPattern   = "(?i)http-equiv=(\"|')refresh(\"|')"
//    let metaRobotsPattern    = "(?i)robots"
//    let metaTagPattern       = "(?i)<meta.+?>"
//    let noFollowPattern      = "(?i)nofollow"
//    let noIndexPattern       = "(?i)noindex"
//    let noneDirecitvePattern = "(?i)none"
//    let oneWordPattern       = "[^0-9\W]+"
//    let relAttributePattern  = "rel=(\"|')(.?)(\"|')"
//    let robotsAllowPattern   = "(?i)^allow:(.+)"
//    let robotsCommentPattern = "^#"
//    let robotsDAllowPattern  = "(?i)^disallow:(.+)"
//    let threeWordsPattern    = "[^0-9\W]+ [^0-9\W]+ [^0-9\W]+"
//    let titleTagPattern      = "(?is)<title>(.+?)</title>"
//    let twoWordsPattern      = "[^0-9\W]+ [^0-9\W]+"
//    let userAgentPattern     = ".+?:"
//    let userAgentPattern'    = "(?i)^user-agent"
//    let userAgentPattern''   = "(?i)^user-agent:(.+)"
//
//    // Regex objects.
//    let absoluteUriRegex   = compileRegex absoluteUriPattern
//    let altAttributeRegex  = compileRegex altAttributePattern
//    let anchorRegex        = compileRegex aTagPattern
//    let commentJSCssRegex  = compileRegex commentJSCssPattern
//    let crawlDelayRegex    = compileRegex crawlDelayPattern
//    let googleBotRegex     = compileRegex googleBotPattern
//    let h1TagRegex         = compileRegex h1TagPattern
//    let hrefRegex          = compileRegex hrefPattern
//    let htmlRegex          = compileRegex htmlContentPattern
//    let htmlTagRegex       = compileRegex htmlTagPattern
//    let imgTagRegex        = compileRegex imgTagPattern
//    let inlineCssRegex     = compileRegex inlineCssPattern
//    let inlineJsRegex      = compileRegex inlineJsPattern
//    let metaContentRegex   = compileRegex metaContentPattern
//    let metaDescContRegex  = compileRegex metaDescContPattern
//    let metaDescRegex      = compileRegex metaDescPattern
//    let metaRefreshRegex   = compileRegex metaRefreshPattern
//    let metaRobotsRegex    = compileRegex metaRobotsPattern
//    let metaTagRegex       = compileRegex metaTagPattern
//    let noFollowRegex      = compileRegex noFollowPattern
//    let noIndexRegex       = compileRegex noIndexPattern
//    let noneRegex          = compileRegex noneDirecitvePattern
//    let oneKeywordRegex    = compileRegex oneWordPattern
//    let relAttributeRegex  = compileRegex relAttributePattern
//    let robotsAllowRegex   = compileRegex robotsAllowPattern
//    let robotsCommentRegex = compileRegex robotsCommentPattern
//    let robotsDAllowRegex  = compileRegex robotsDAllowPattern
//    let threeKeywordsRegex = compileRegex threeWordsPattern
//    let titleTagRegex      = compileRegex titleTagPattern
//    let twoKeywordsRegex   = compileRegex twoWordsPattern
//    let userAgentRegex     = compileRegex userAgentPattern
//    let userAgentRegex'    = compileRegex userAgentPattern''
//
//    /// Compiles an internal link regex for the given URI host.
//    let internalLinkRegex host =
//        let host' = Regex.Escape host
//        let pattern = "(?i)^https?://" + host'
//        compileRegex pattern
//
//    /// Splits a string list at elements that satisfy a regex pattern.
//    let splitAtPattern pattern lst =
//        let strLst = List.empty
//        let regex  = compileRegex pattern
//        let rec split lst acc b strLst =
//            match lst with
//                | h :: t ->
//                    let isMatch = regex.IsMatch h
//                    match isMatch with
//                        | true ->
//                            match b with
//                                | true ->
//                                    let acc' = h :: acc
//                                    split t acc' false strLst
//                                | false ->
//                                    let acc' = List.rev acc
//                                    let strLst' = acc' :: strLst
//                                    split t [h] true strLst'
//                        | false ->
//                            let acc' = h :: acc
//                            split t acc' false strLst
//                | [] ->
//                    let acc' = List.rev acc
//                    acc' :: strLst
//        split lst [] true strLst
//
//    /// Attempts to match a string with a regex.
//    let matchOption (regex : Regex) str =
//        let regexMatch = regex.Match str
//        match regexMatch.Success with
//            | true  -> Some regexMatch
//            | false -> None
//
//    /// Returns the trimmed value of a group in a match object.
//    let groupValue (regexMatch : Match) (idx : int) = regexMatch.Groups.[idx].Value.Trim()
//
//    /// Returns the count of a match collection converted to float.
//    let matchCount (matchCollection : Match seq) = matchCollection |> Seq.length |> float
//
//    /// Matches a sting with a regex pattern.
//    let regexMatches (regex : Regex) str = regex.Matches str |> Seq.cast<Match>
//
//    let altAttribute (imgTagMatch : Match) =
//        let value = imgTagMatch.Value
//        let index = imgTagMatch.Index
//        let altMatch = altAttributeRegex.Match value
//        altMatch.Success |> function
//            | true ->
//                let alt = groupValue altMatch 2 |> Some
//                value, index, alt
//            | false -> value, index, None
//
//    let constructViolation v i =
//        {
//            V = v
//            Index = i
//        }
//
//    let constructViolation' v i description (arg : obj) =
//        let violation = constructViolation v i
//        let description = String.Format(description, arg)
//        {
//            violation with
//                V = { violation.V with Description = description }
//        }
//
//    let constructViolation'' v i description (args : obj []) =
//        let violation = constructViolation v i
//        let description = String.Format(description, args)
//        {
//            violation with
//                V = { violation.V with Description = description }
//        }
//
//    let w3cValidatorUri = "http://validator.w3.org/check?uri="
//
//    /// Decodes HTML encoded characters..
//    let decodeHtml html = WebUtility.HtmlDecode html
//
//    /// Calculates the density of a keyword.
//    let computeDensity count count' length = float count / count' * 100. * length
//
//    /// Returns the host of a URL.
//    let hostFromUrl url =
//        let uri = Uri url
//        uri.Host
//
//    /// Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
//    let awaitHttpResponse (client : HttpClient) (requestUri : Uri) = client.GetAsync requestUri |> Async.AwaitTask
//
//    /// Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
//    let awaitHttpResponse' (client : HttpClient) (requestUrl : string) = client.GetAsync requestUrl |> Async.AwaitTask
//    
//    /// Returns an asynchronous computation that will wait for the task of reading the content of a HTTP response message as a string.
//    let awaitReadAsString (response : HttpResponseMessage) = response.Content.ReadAsStringAsync() |> Async.AwaitTask
//
//    /// IE9's user-agent string.
//    let ie9UserAgent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"
//
//    /// Initializes a HttpClient instance, sets its timeout property to the specified value and adds a user-agent request header. 
//    let httpClient timeout =
//        let client = new HttpClient()
//        client.Timeout <- TimeSpan(0, 0, timeout)
//        client.DefaultRequestHeaders.Add("user-agent", ie9UserAgent)
//        client