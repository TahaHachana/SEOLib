namespace SEOLib

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.RegularExpressions
open System.Web
open System.Xml.Linq
open Google.Apis.Pagespeedonline.v1.Data
open Types

module internal Utilities =

    [<AutoOpenAttribute>]
    module Html =

        /// Decodes HTML encoded characters.
        let decodeHtml html = WebUtility.HtmlDecode html

        let checkEmptyString str = match str with "" -> None | _ -> Some str
        
        let checkEmptyString' = decodeHtml >> checkEmptyString

        ///Compiles a pattern into a Regex object.
        let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

        /// Returns the trimmed value of a group in a match object.
        let inline groupValue (idx : int) (regex : Regex) str =
            regex.Match(str).Groups.[idx].Value.Trim()

        let inline groupValue' (idx : int) (matchObj : Match) =
            matchObj.Groups.[idx].Value.Trim()

        let fstGroupValue regex str = groupValue 0 regex str
        let sndGroupValue regex str = groupValue 1 regex str
        let trdGroupValue regex str = groupValue 2 regex str
        let frtGroupValue regex str = groupValue 3 regex str

        let fstGroupValue' matchObj = groupValue' 0 matchObj
        let sndGroupValue' matchObj = groupValue' 1 matchObj
        let trdGroupValue' matchObj = groupValue' 2 matchObj
        let frtGroupValue' matchObj = groupValue' 3 matchObj

        let attributePattern   = "(?i)(\w+)=(\"|')(.+?)(\"|')\W"
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

        let makeHtmlAttribute key value : HtmlAttribute =
            {
                Key   = key
                Value = value
            }

        let tagAttributes tag =
            attributeRegex.Matches tag
            |> Seq.cast<Match>
            |> Seq.toArray
            |> Array.map (fun x ->
                makeHtmlAttribute (sndGroupValue' x) (frtGroupValue' x))

        let metaTag metaTags (regex : Regex) (regex' : Regex) =
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
        let ie10UserAgent = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)"
    
        /// Initializes a HttpClient instance and adds a user-agent request header. 
        let httpClient () =
            let client = new HttpClient()
            client.DefaultRequestHeaders.Add("User-Agent", ie10UserAgent)
            client.MaxResponseContentBufferSize <- int64 1073741824
            client

        /// Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
        let awaitHttpResponse (client : HttpClient) (requestUri : Uri) = client.GetAsync requestUri |> Async.AwaitTask

        /// Returns an asynchronous computation that will wait for the task of reading the content of a HTTP response message as a string.
        let awaitReadAsString (httpContent : HttpContent) = httpContent.ReadAsStringAsync() |> Async.AwaitTask

        /// Initializes a Header instance.
        let constructHeader key (value : string seq) = {Key = key; Value = Seq.toList value}
        
        let httpHeaders (responseHeaders : HttpResponseHeaders) (contentHeaders : HttpContentHeaders) =
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

//        let stopWords = Array.toList englishStopWords

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

        let commentJSCssPattern = "(?is)(<!--.*?--\s*>|<script.*?</script>|<style.*?</style>)"
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

        let absoluteUriPattern  = "(?i)^https?://[^\"]*"
        let aTagPattern         = "(?is)(<a .+?>)(.+?)</a>"
        let baseTagPattern      = "(?is)<base.+?>"
        let commentPattern      = "(?s)<!--.*?--\s*>"
        let hrefPattern         = "(?i) href\\s*=\\s*(\"|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"'\#]+)(\"|'|\#)"
        let relAttributePattern = "rel=(\"|')(.+?)(\"|')"
        let noFollowPattern     = "(?i)nofollow"
        let inlineJsCssPattern  = "(?is)(<script.*?</script>|<style.*?</style>)"

        let absoluteUriRegex  = compileRegex absoluteUriPattern
        let aTagRegex         = compileRegex aTagPattern
        let baseTagRegex      = compileRegex baseTagPattern
        let commentRegex      = compileRegex commentPattern
        let hrefRegex         = compileRegex hrefPattern    
        let relAttributeRegex = compileRegex relAttributePattern
        let noFollowRegex     = compileRegex noFollowPattern
        let inlineJsCssRegex  = compileRegex inlineJsCssPattern

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

    [<AutoOpen>]
    module Validator =

        let validatorUri = "http://validator.w3.org/check?uri="

        let buildUri (uriString : string) =
            let uriString' = HttpUtility.UrlEncode uriString
            String.concat "" [validatorUri; uriString'; "&output=soap12"]

        let loadXdocument (uriString : string) =
            try
                XDocument.Load uriString |> Some
            with _ -> None

        let findByLocalname (xelements : XElement seq) localname =
            xelements
            |> Seq.find (fun x -> x.Name.LocalName = localname)
            |> fun x -> x.Value

        let tryFindByLocalname (xelements : XElement seq) localname =
            xelements
            |> Seq.tryFind (fun x -> x.Name.LocalName = localname)
            |> function
                | None   -> None
                | Some x -> Some x.Value
    
        let filterByLocalname (xelements : XElement seq) localname =
            xelements
            |> Seq.filter (fun x -> x.Name.LocalName = localname)
            |> Seq.toList

        let makeMarkupError line col message messageId explanation source =
            {
                Line        = line
                Col         = col
                Message     = message
                MessageId   = messageId
                Explanation = explanation
                Source      = source
            }

        let optionValue (x : 'T option) = x.Value

        let collectMarkupErrorData (xelement : XElement) =
            let descendants = xelement.Descendants ()
            let findByLocalname' = tryFindByLocalname descendants
            let line = findByLocalname' "line"
            let col = findByLocalname' "col"
            let message = findByLocalname' "message" |> optionValue
            let messageId = findByLocalname' "messageid" |> optionValue
            let explanation = findByLocalname' "explanation"
            let source = findByLocalname' "source"
            makeMarkupError line col message messageId explanation source
    
        let collectMarkupErrors xelements localname count =
            match count with
                | 0 -> None
                | _ ->
                    filterByLocalname xelements localname
                    |> List.map collectMarkupErrorData
                    |> function
                        | []  -> None
                        | lst -> Some lst

        let makeMarkupValidation doctype charset status errorCount warningCount errors warnings =
            {
                Doctype      = doctype
                Charset      = charset
                Status       = status
                ErrorCount   = errorCount
                WarningCount = warningCount
                Errors       = errors
                Warnings     = warnings
            }

    [<AutoOpen>]
    module PageSpeed =

        let getPageStats (result : Result) = result.PageStats

        let testNull = function null -> 0 | (x : string) -> int x
    
        let testNullable (nullable : Nullable<'T>) =
            match nullable.HasValue with
                | false -> None
                | true  -> Some nullable.Value

        let getCssBytes (stats : Result.PageStatsData) = testNull stats.CssResponseBytes
        let getFlashBytes (stats : Result.PageStatsData) = testNull stats.FlashResponseBytes
        let getHtmlBytes (stats : Result.PageStatsData) = testNull stats.HtmlResponseBytes
        let getImageBytes (stats : Result.PageStatsData) = testNull stats.ImageResponseBytes
        let getJavascriptBytes (stats : Result.PageStatsData) = testNull stats.JavascriptResponseBytes
        let getNumberCssResource (stats : Result.PageStatsData) = testNullable stats.NumberCssResources
        let getNumberHosts (stats : Result.PageStatsData) = testNullable stats.NumberHosts
        let getNumberJsResources (stats : Result.PageStatsData) = testNullable stats.NumberJsResources
        let getNumberResources (stats : Result.PageStatsData) = testNullable stats.NumberResources
        let getNumberStaticResourecs (stats : Result.PageStatsData) = testNullable stats.NumberStaticResources
        let getNumberTextBytes (stats : Result.PageStatsData) = testNull stats.TextResponseBytes
        let getOtherBytes(stats : Result.PageStatsData) = testNull stats.OtherResponseBytes
        let getTotalRequestBytes (stats : Result.PageStatsData) = testNull stats.TotalRequestBytes

        let makePagespeedStats cssBytes flashBytes htmlBytes imageBytes javascriptBytes numberCssResources numberHosts numberJsResources numberResources numberStaticResources numberTextBytes otherBytes totalBytes =
            {
                CssBytes              = cssBytes
                FlashBytes            = flashBytes
                HtmlBytes             = htmlBytes
                ImageBytes            = imageBytes
                JavascriptBytes       = javascriptBytes
                NumberCssResources    = numberCssResources
                NumberHosts           = numberHosts
                NumberJsResources     = numberJsResources
                NumberResources       = numberResources
                NumberStaticResourecs = numberStaticResources
                NumberTextBytes       = numberTextBytes
                OtherBytes            = otherBytes
                TotalRequestBytes     = totalBytes
            }

        let testNull' = function null -> None | x -> Seq.toList x |> Some

        let regex = compileRegex "\$\d+"

        let rec updateString str (lst : string list) idx =
            match lst with
            | h :: t ->
                let str' = regex.Replace(str, h, 1)
                let idx' = idx + 1
                updateString str' t idx'
            | [] -> str

        let makeSuggestion header urls =
            {
                Header = header
                Urls   = urls
            }

        let nullToOption = function null -> None | x -> Some x

        let formatUrlData (urlData : Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema.UrlBlocksData.UrlsData.ResultData) =
            let format = urlData.Format      
            let argsOption = nullToOption urlData.Args
            match argsOption with
                | None -> format
                | Some args ->
                    let args' = args |> Seq.map (fun x -> x.Value) |> Seq.toList
                    updateString format args' 0

        let getUrlData (urls : seq<Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema.UrlBlocksData.UrlsData>) =
            match urls with
                | null -> []
                | urls ->
                    urls
                    |> Seq.toList
                    |> List.map (fun x -> x.Result)
            |> List.map formatUrlData

        let formatUrlBlocksData (urlBlocksData : Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema.UrlBlocksData) =
            let header = urlBlocksData.Header
            let argsOption = nullToOption header.Args
            let format = header.Format
            let format' =
                match argsOption with
                    | None -> format
                    | Some args ->
                        let args' = args |> Seq.map (fun x -> x.Value) |> Seq.toList
                        updateString format args' 0
            let urlData = getUrlData urlBlocksData.Urls
            makeSuggestion format' urlData

        let makePageSpeedRule (ruleResult : Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema) =
            let name = ruleResult.LocalizedRuleName
            let impact = testNullable ruleResult.RuleImpact
            let score = testNullable ruleResult.RuleScore
            let suggestions =
                testNull' ruleResult.UrlBlocks
                |> function
                    | None -> None
                    | Some lst ->
                        lst
                        |> List.map formatUrlBlocksData
                        |> Some
            {
                Name        = name
                Impact      = impact
                Score       = score
                Suggestions = suggestions
            }