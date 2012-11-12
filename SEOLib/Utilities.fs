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
        let metaTagPattern     = "(?i)<meta.+?>"
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
            let html' = commentJSCssRegex.Replace(html, " ")
            let html'' = tagRegex.Replace(html', "") |> decodeHtml
            html''.ToLower()

    [<AutoOpenAttribute>]
    module Links =

        let absoluteUriPattern = "(?i)^https?://[^\"]*"
        let aTagPattern = "(?is)(<a.+?>)(.+?)</a>"
        let baseTagPattern = "(?is)<base.+?>"
        let commentPattern = "(?s)<!--.*?--\s*>"
        let hrefPattern = "(?i)href\\s*=\\s*(\"|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"'\#]+)(\"|'|\#)"
        let relAttributePattern = "rel=(\"|')(.+?)(\"|')"
        let noFollowPattern = "(?i)nofollow"
    
        let absoluteUriRegex = compileRegex absoluteUriPattern
        let aTagRegex = compileRegex aTagPattern
        let baseTagRegex = compileRegex baseTagPattern
        let commentRegex = compileRegex commentPattern
        let hrefRegex = compileRegex hrefPattern    
        let relAttributeRegex = compileRegex relAttributePattern
        let noFollowRegex = compileRegex noFollowPattern

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

        let linkAnchor matchObj =
            trdGroupValue' matchObj
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

        let removeComments html = commentRegex.Replace(html, "")

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