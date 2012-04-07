namespace SEOLib

open System
open System.Net
open System.Text.RegularExpressions
open SEOLib.HTML
open SEOLib.Types
open SEOLib.Utilities

module Violations =

    // Headings.
    let altMissingHead             = "The ALT attribute of the <img> tag is missing."
    let altEmptyHead               = "The ALT attribute of the <img> tag is empty."
    let descriptionEmptyHead       = "The description is empty."
    let descriptionLongHead        = "The description is too long."
    let descriptionMissingHead     = "The description is missing."
    let descriptionMultipleHead    = "The page contains multiple descriptions."
    let descriptionShortHead       = "The description is too short."
    let h1EmptyHead                = "The <h1> tag is empty."
    let h1MissingHead              = "The <h1> tag is missing."
    let h1MultipleHead             = "The page contains multiple <h1> tags."
    let invalidMarkupHead          = "The page contains invalid markup."
    let largeInlineCssHead         = "The page contains a large number of Cascading Style Sheet (CSS) definitions."
    let largeInlineScriptHead      = "The page contains a large amount of script code."
    let noIndexHead                = "The page was excluded by a noindex attribute."
    let queryParameterCountHead    = "The URL contains too many query string arguments."
    let titleEqualsDescriptionHead = "The title and description are identical "
    let titleEmptyHead             = "The title is empty."
    let titleLongHead              = "The title is too long."
    let titleMissingHead           = "The title is missing."
    let titleShortHead             = "The title is too short."
    let manyLinksHead              = "The page contains too many hyperlinks."
    let refreshRedirectHead        = "The page uses a refresh definition instead of using redirection."

    // Descriptions.
    let altMissingDesc             = "The ALT attribute is used to provide relevant information about the image to the search engines."
    let altEmptyDesc               = altMissingDesc
    let descriptionEmptyDesc       = "The <meta name=\"description\" /> tag does not have any content. The description meta tag is used to provide a short description of the page content."
    let descriptionLongDesc        = "The content within the <meta name=\"description\"> tag is too long ({0} characters). The description must not exceed 150 characters."
    let descriptionMissingDesc     = "The page markup does not have the <meta name=\"description\" /> tag."
    let descriptionMultipleDesc    = "The page markup has {0} <meta name=\"description\" /> tag."
    let descriptionShortDesc       = "The content in the <meta name=\"description\"> tag is too short ({0} characters). The description must be at least 25 characters long."
    let h1EmptyDesc                = "An <h1> tag is an indicator of what the page content is about."
    let h1MissingDesc              = h1EmptyDesc
    let h1MultipleDesc             = "An <h1> tag is an indicator of what the page content is about. Search engines diminish the value of a page that contains multiple <h1> tags."
    let invalidMarkupDesc          = "The page contains {0} error(s) and {1} warning(s)."
    let largeInlineCssDesc         = "The page contains a large block of embedded Cascading Style Sheet (CSS) code ({0} characters). Search engines will ignore CSS code, but large quantities of CSS code that precede the actual text content of the page will force the text content further down in the HTML."
    let largeInlineScriptDesc      = "The page contains a large block of embedded script code ({0} characters). Search engines will ignore script code, but large quantities of script will force the actual text content of the page further down in the HTML."
    let noIndexDesc                = "The page content at this URL has not been analyzed because it's disallowed for indexing."
    let queryParameterCountDesc    = "The URL has {0} query string parameters. Most search engines will analyze up to four URL parameters and ignore any additional parameters."
    let titleEqualsDescriptionDesc = "The <title> and the <meta name=\"description\"> tags of the page contain identical text."
    let titleEmptyDesc             = "The <title> tag of the page is empty. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
    let titleLongDesc              = "The text in the <title> tag is too long ({0} characters). The title must not exceed 65 characters, including spaces. Most search engines will truncate the text in the <title> tag after a fixed number of characters."
    let titleMissingDesc           = "The page markup does not have the <title> tag. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
    let titleShortDesc             = "The text in the <title> tag is too short ({0} characters). The title must contain at least five (5) characters. A title that is too short can affect how the content of the page is indexed, ranked, and presented by search engines."
    let manyLinksDesc              = "The page contains {0} links. Too many links in the same page may affect the indexing process that is used by search engine crawlers. This may result in poor search rankings for the page or in the search engine ignoring the page."
    let refreshRedirectDesc        = "The page uses a meta refresh tag to implement URL redirection. Using meta refresh syntax is discouraged by the W3C, because unexpected refresh may confuse users and may impair the \"back\" button functionality in some Web browsers."

    // Recommendations.
    let altMissingRecom             = "Add an \"alt\" attribute to the <img> tag."
    let altEmptyRecom               = "Add content to the \"alt\" attribute."
    let descriptionEmptyRecom       = "Modify the <meta name=\"description\" /> tag in the <head> section of the page to contain the actual page content description."
    let descriptionLongRecom        = "The description must be between 25 and 150 characters long, human readable, actionable, and rich in keywords."
    let descriptionMissingRecom     = "Add <meta name=\"description\" content=\"description_text\"> to the <head> section of the page."
    let descriptionMultipleRecom    = "Remove extra <meta name='description' /> tags from the page."
    let descriptionShortRecom       = descriptionLongRecom
    let h1EmptyRecom                = "Add a keyword rich heading inside the <h1> tag."
    let h1MissingRecom              = "Add an <h1> tag to the <body> section of the page."
    let h1MultipleRecom             = "Use only one <h1> tag per page."
    let invalidMarkupRecom          = "Analyze the markup for errors."
    let largeInlineCssRecom         = "Move large blocks of CSS code to externally linked CSS files."
    let largeInlineScriptRecom      = "Move all inline script code to externally linked script files."
    let noIndexRecom                = ""
    let queryParameterCountRecom    = "Consider reducing the number of query string parameters, or use a URL Rewrite Module to simplify the URL structure."
    let titleEqualsDescriptionRecom = descriptionLongRecom
    let titleEmptyRecom             = "Add the title text inside the <title> tag of the page."
    let titleLongRecom              = "The title must be between 5 and 65 characters long."
    let titleMissingRecom           = "Add the <title> tag inside of the <head> section of the page."
    let titleShortRecom             = titleLongRecom
    let manyLinksRecom              = "Reduce the number of links on the page to fewer than 250."
    let refreshRedirectRecom        = "Use HTTP Redirection instead of using HTTP Refresh headers or meta refresh tags."

    // Violations.

    /// Alt empty violation.
    let altEmpty =
        {
            Category       = SEO
            Code           = AltEmpty
            Description    = altEmptyDesc
            Heading        = altEmptyHead
            Level          = Warning
            Recommendation = altEmptyRecom
        }

    /// Alt missing violation.
    let altMissing =
        {
            Category       = SEO
            Code           = AltMissing
            Description    = altMissingDesc
            Heading        = altMissingHead
            Level          = Warning
            Recommendation = altMissingRecom
        }

    /// Description empty violation.
    let descriptionEmpty =
        {
            Category       = SEO
            Code           = DescriptionEmpty
            Description    = descriptionEmptyDesc
            Heading        = descriptionEmptyHead
            Level          = Warning
            Recommendation = descriptionEmptyRecom
        }

    /// Description long violation.
    let descriptionLong =
        {
            Category       = SEO
            Code           = DescriptionLong
            Description    = descriptionLongDesc
            Heading        = descriptionLongHead
            Level          = Warning
            Recommendation = descriptionLongRecom
        }

    /// Description missing violation.
    let descriptionMissing =
        {
            Category       = SEO
            Code           = DescriptionMissing
            Description    = descriptionMissingDesc
            Heading        = descriptionMissingHead
            Level          = Warning
            Recommendation = descriptionMissingRecom
        }

    /// Description multiple violation.
    let descriptionMultiple =
        {
            Category       = Standards
            Code           = DescriptionMultiple
            Description    = descriptionMultipleDesc
            Heading        = descriptionMultipleHead
            Level          = Warning
            Recommendation = descriptionMultipleRecom
        }

    /// Description short violation.
    let descriptionShort =
        {
            Category       = SEO
            Code           = DescriptionShort
            Description    = descriptionShortDesc
            Heading        = descriptionShortHead
            Level          = Warning
            Recommendation = descriptionShortRecom
        }

    /// H1 empty.
    let h1Empty =
        {
            Category       = SEO
            Code           = H1Empty
            Description    = h1EmptyDesc
            Heading        = h1EmptyHead
            Level          = Warning
            Recommendation = h1EmptyRecom
        }

    /// H1 missing violation.
    let h1Missing =
        {
            Category       = SEO
            Code           = H1Missing
            Description    = h1MissingDesc
            Heading        = h1MissingHead
            Level          = Warning
            Recommendation = h1MissingRecom
        }

    /// H1 multiple violation.
    let h1Multiple =
        {
            Category       = Standards
            Code           = H1Multiple
            Description    = h1MultipleDesc
            Heading        = h1MultipleHead
            Level          = Warning
            Recommendation = h1MultipleRecom
        }

    /// Invalid markup violation.
    let invalidMarkup =
        {
            Category       = Content
            Code           = InvalidMarkup
            Description    = invalidMarkupDesc
            Heading        = invalidMarkupHead
            Level          = Warning
            Recommendation = invalidMarkupRecom
        }

    /// Large inline CSS violation.
    let largeInlineCss =
        {
            Category       = Performance
            Code           = LargeInlineCss
            Description    = largeInlineCssDesc
            Heading        = largeInlineCssHead
            Level          = Warning
            Recommendation = largeInlineCssRecom
        }

    /// Large inline script violation.
    let largeInlineScript =
        {
            Category       = Performance
            Code           = LargeInlineScript
            Description    = largeInlineScriptDesc
            Heading        = largeInlineScriptHead
            Level          = Warning
            Recommendation = largeInlineScriptRecom
        }

    /// No index violation.
    let noIndex =
        {
            Category       = SEO
            Code           = NoIndex
            Description    = noIndexDesc
            Heading        = noIndexHead
            Level          = Information
            Recommendation = noIndexRecom
        }

    /// Query string parameter count violation.
    let queryParameterCount =
        {
            Category       = SEO
            Code           = QueryStringParameterCount
            Description    = queryParameterCountDesc
            Heading        = queryParameterCountHead
            Level          = Warning
            Recommendation = queryParameterCountRecom
        }

    /// Title equals description violation.
    let titleEqualsDescription =
        {
            Category       = SEO
            Code           = TitleAndDescriptionEquals
            Description    = titleEqualsDescriptionDesc
            Heading        = titleEqualsDescriptionHead
            Level          = Warning
            Recommendation = titleEqualsDescriptionRecom
        }

    /// Empty title violation.
    let titleEmpty =
        {
            Category       = SEO
            Code           = TitleEmpty
            Description    = titleEmptyDesc
            Heading        = titleEmptyHead
            Level          = Error
            Recommendation = titleEmptyRecom
        }

    /// Long title violation.
    let titleLong =
        {
            Category       = SEO
            Code           = TitleLong
            Description    = titleLongDesc
            Heading        = titleLongHead
            Level          = Warning
            Recommendation = titleLongRecom
        }

    /// Missing title violation.
    let titleMissing =
        {
            Category       = SEO
            Code           = TitleMissing
            Description    = titleMissingDesc
            Heading        = titleMissingHead
            Level          = Error
            Recommendation = titleMissingRecom
        }

    /// Short title violation.
    let titleShort =
        {
            Category       = SEO
            Code           = TitleShort
            Description    = titleShortDesc
            Heading        = titleShortHead
            Level          = Warning
            Recommendation = titleShortRecom
        }

    /// Too many links violation.
    let tooManyLinks =
        {
            Category       = SEO
            Code           = TooManyLinks
            Description    = manyLinksDesc
            Heading        = manyLinksHead
            Level          = Error
            Recommendation = manyLinksRecom
        }

    /// Refresh to redirect violation.
    let refreshToRedirect =
        {
            Category       = Standards
            Code           = UseOfRefreshToRedirect
            Description    = refreshRedirectDesc
            Heading        = refreshRedirectHead
            Level          = Warning
            Recommendation = refreshRedirectRecom
        }

    // Violation detection functions.

    /// Finds missing/empty alt attribute violations.
    let checkAltEmpty html =
        async {
            let arr =
                regexMatches imgTagRegex html
                |> Seq.toArray
                |> Array.map altAttribute
                |> Array.partition (fun (_, _, alt) -> alt.IsSome)
                |> function
                    | someAlt, noneAlt ->
                        let someAlt' =
                            someAlt
                            |> Array.filter (fun (_, _, alt) -> alt.Value.Length  = 0)
                            |> Array.map (fun (_, index, _) -> constructViolation altEmpty <| Some index)
                        let noneAlt' = noneAlt |> Array.map (fun (_, index, _) ->
                            constructViolation altMissing <| Some index)
                        Array.append someAlt' noneAlt'
            return arr
        }

    /// Finds description related violations.
    let checkDescription html =
        async {
            let tags = metaTags html
            let desc = metaDesc tags
            let metaDescCount = desc.Length
            let arr =
                match metaDescCount with
                    | _ when metaDescCount > 1 ->
                        [| constructViolation' descriptionMultiple None descriptionMultipleDesc metaDescContent |]
                    | _ when metaDescCount = 0 -> [| constructViolation descriptionMissing  None |]
                    | _ ->
                        let metaDesc, index = desc.[0]
                        let index' = Some index
                        let content = metaDescContent metaDesc
                        let contentLength = content.Length
                        match contentLength with
                            | _ when contentLength > 150 ->
                                [| constructViolation' descriptionLong index' descriptionLongDesc contentLength |]
                            | _ when contentLength < 25  ->
                                [| constructViolation' descriptionLong index' descriptionLongDesc contentLength |]
                            | _ when contentLength = 0   -> [| constructViolation descriptionEmpty <| Some index |]
                            | _                          -> [||]
            return arr
            }

    /// Finds H1 related violations.
    let checkH1 html =
        async {
            let h1s = h1Tags html
            let h1Count = Seq.length h1s
            let arr =
                match h1Count with
                    | 0 -> [| constructViolation h1Missing None|]
                    | 1 ->
                        let heading, index = h1s |> Seq.nth 0 |> (fun x -> x.Value, x.Index)
                        let headingLength = heading.Length
                        match headingLength with
                            | 0 -> [| constructViolation h1Empty <| Some index |]
                            | _ -> [||]
                    | _ -> [| constructViolation h1Multiple None |]
            return arr
        }

    /// Validites the markup of a Web page.
    let checkMarkup url =
        async {
            let! status = validationStatus url
            let arr =
                match status with
                | Abort | Valid -> [||]
                | Invalid (errors, warnings) ->
                    [| constructViolation'' invalidMarkup None invalidMarkupDesc [|errors; warnings|]|]
            return arr
            }

    let f regex  html v description =
        regexMatches regex html
        |> Seq.toArray
        |> Array.map (fun x -> x.Value, x.Index)
        |> Array.filter (fun (value, _) -> value.Length > 1024)
        |> Array.map (fun (value, index) ->
            let index' = Some index
            let length = value.Length
            constructViolation' v index' description length)

    /// Finds large blocks of inline CSS.
    let checkInlineCss html =
        async {
            let inlineCss = f inlineCssRegex html largeInlineCss largeInlineCssDesc
            return inlineCss
            }

    /// Finds large blocks of inline JavaScript.
    let checkInlineJS html =
        async {
            let inlineJS = f inlineJsRegex html largeInlineScript largeInlineScriptDesc
            return inlineJS
            }

    /// Checks if a Web page is disallowed for crawling.
    let checkNoIndex (wp : WebPage) =
        async {
            let indexing = wp.Robots.Indexing
            match indexing with
                | Allowed    -> return [||]
                | Disallowed -> return [| constructViolation noIndex None |]
            }

    /// Checks query parameters in a URL.
    let checkQueryString (uri : Uri) =
        async {
            let query = uri.Query
            let parameterRegex = compileRegex "(\?|&)[^=]+"
            let arr =
                regexMatches parameterRegex query
                |> Seq.length
                |> function
                    | x when x > 4 -> [| constructViolation queryParameterCount None |]
                    | _            -> [||]
            return arr
        }

    /// Finds title related violations.
    let checkTitle html =
        async {
            let titles = titleTags html
            let titlesCount = titles.Length
            let arr =
                match titlesCount with
                    | _ when titlesCount = 0 -> [| constructViolation titleMissing None |]
                    | _                      ->
                        let title, index = List.head titles
                        let index' = Some index
                        let titleLength = title.Length
                        let arr' =
                            match titleLength with
                                | _ when titleLength > 65 ->
                                    [| constructViolation' titleLong index' titleLongDesc titleLength |]
                                | _ when titleLength < 5  ->
                                    [| constructViolation' titleShort index' titleLongDesc titleLength |]
                                | _ when titleLength = 0  ->
                                    [| constructViolation titleEmpty <| Some index |]
                                | _                       -> [||]
                    
                        let tags = metaTags html
                        let desc = metaDesc tags |> function
                                | [] -> ""
                                | lst -> lst |> List.head |> fst |> metaDescContent
                        let titleEqualsDesc = title.ToLower() = desc.ToLower()
                        let arr'' = 
                            match titleEqualsDesc with
                                | true  -> [| constructViolation titleEqualsDescription None |]
                                | false -> [||]
                        Array.append arr' arr''
            return arr
            }

    /// Checks if there are too many links in a Web page.
    let checkLinks html =
        async {
            let anchorsCount = regexMatches anchorRegex html |> Seq.length
            let arr =
                match anchorsCount with
                    | _ when anchorsCount > 250 ->
                        [| constructViolation' tooManyLinks None manyLinksDesc anchorsCount |]
                    | _                         -> [||]
            return arr
            }

    /// Checks if a Web page contains a refresh meta tag.
    let checkMetaRefresh html =
        async {
            let tags = metaTags html
            let metaRef = metaRefresh tags
            let arr =
                match metaRef with
                    | [] -> [||]
                    | _  -> [| constructViolation refreshToRedirect None |]
            return arr
            }