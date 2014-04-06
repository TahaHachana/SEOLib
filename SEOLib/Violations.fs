module SEOLib.Violations

open System
open System.Text.RegularExpressions

type ViolationLevel = Error | Warning

type ViolationCategory =
    | Content
    | Performance
    | SEO
    | Standards

type ViolationCode =
    | AltEmpty
    | AltMissing
    | DescriptionEmpty
    | DescriptionLong
    | DescriptionMissing
    | DescriptionMultiple
    | DescriptionShort
    | H1Empty
    | H1Missing
    | H1Multiple
    | LargeInlineCss
    | LargeInlineScript
    | QueryParameterCount
    | RefreshToRedirect
    | TitleEqDescription
    | TitleEmpty
    | TitleLong
    | TitleMissing
    | TitleShort
    | TooManyLinks

type private ViolationPrivate =
    {
        Category : ViolationCategory
        Code : ViolationCode
        Description : string
        Heading : string
        Index : int option
        Level : ViolationLevel
        Recommendation : string
    }

type Violation =
    {
        Category : ViolationCategory
        Code : ViolationCode
        Description : string
        Heading : string
        Position : Position option
        Level : ViolationLevel
        Recommendation : string
    }

and Position =
    {
        Line : int
        Column : int
    }

    static member New line column =
        {
            Line = line
            Column = column
        }

[<AutoOpen>]
module private Utils =

    let altMissing idx =
        {
            Category = SEO
            Code = AltMissing
            Description = "The \"alt\" attribute is used to provide relevant information about the image to search engines."
            Heading = "The \"alt\" attribute of the <img> tag is missing."
            Index = Some idx
            Level = Warning
            Recommendation = "Add an \"alt\" attribute that describes the image."
        }

    let altEmpty idx =
        {
            Category = SEO
            Code = AltEmpty
            Description = "The \"alt\" attribute is used to provide relevant information about the image to search engines."
            Heading = "The \"alt\" attribute of the <img> tag is empty."
            Index = Some idx
            Level = Warning
            Recommendation = "Add content to the \"alt\" attribute."
        }

    let checkAlt (imgMatch:Match) =
        let value, idx = imgMatch.Value, imgMatch.Index
        let altMatch = Regex.altAttr.Match value
        match altMatch.Success with
        | false -> Some <| altMissing idx
        | true ->
            match altMatch.Groups.[2].Value.Length with
            | 0 -> Some <| altEmpty idx
            | _ -> None

    let altViolations html =
        [
            for x in Regex.img.Matches html do
                yield checkAlt x
        ]

    let descriptionEmpty idx =
        {
            Category = SEO
            Code = DescriptionEmpty
            Description = "The meta description tag is empty. The description meta tag is used to provide a short description of the page."
            Heading = "The meta description is empty."
            Index = Some idx
            Level = Warning
            Recommendation = "Modify the meta description tag in the <head> section of the page to contain the page's description."
        }

    let descriptionLong length idx =
        {
            Category = SEO
            Code = DescriptionLong
            Description = sprintf "The content of the meta description tag is too long (%d characters). The description must not exceed 150 characters." length
            Heading = "The meta description is too long."
            Index = Some idx
            Level = Warning
            Recommendation = "The description must be between 25 and 150 characters long, human readable, actionable, and rich in keywords."
        }

    let descriptionShort length idx =
        {
            Category = SEO
            Code = DescriptionShort
            Description = sprintf "The content of the meta description tag is too short (%d characters). The description must be at least 25 characters long." length
            Heading = "The meta description is too short."
            Index = Some idx
            Level = Warning
            Recommendation = "The description must be between 25 and 150 characters long, human readable, actionable, and rich in keywords."
        }

    let checkMetaDescription metaDescription idx =
        let contentMatch = Regex.contentAttr.Match metaDescription
        match contentMatch.Success with
        | false -> Some <| descriptionEmpty idx
        | true ->
            match contentMatch.Groups.[2].Value.Length with
            | x when x = 0 -> Some <| descriptionEmpty idx
            | x when x > 150 -> Some <| descriptionLong x idx
            | x when x < 25 -> Some <| descriptionShort x idx
            | _ -> None

    let descriptionMissing =
        {
            Category = SEO
            Code = DescriptionMissing
            Description = "The page's markup does not contain the meta description tag."
            Heading = "The meta description is missing."
            Index = None
            Level = Warning
            Recommendation = "Add a meta description tag to the <head> section of the page."
        }

    let descriptionMultiple count =
        {
            Category = Standards
            Code = DescriptionMultiple
            Description = sprintf "The page's markup contains %d meta description tags." count
            Heading = "The page contains multiple meta description tags."
            Index = None
            Level = Warning
            Recommendation = "Keep a single meta description tag on the page."
        }

    let matchMetaDescription html =
        Regex.meta.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> x.Index, x.Value)
        |> List.filter (snd >> Regex.nameDescription.IsMatch)

    let metaDescriptionViolation html =
        let metaDescriptionTags = matchMetaDescription html
        match metaDescriptionTags.Length with
        | x when x = 0 -> Some descriptionMissing
        | x when x > 1 -> Some <| descriptionMultiple x
        | _ ->
            let idx, metaDescription = metaDescriptionTags.Head
            checkMetaDescription metaDescription idx

    let h1Missing =
        {
            Category = SEO
            Code = H1Missing
            Description = "The <h1> heading summarizes the page’s topic."
            Heading = "The <h1> tag is missing."
            Index = None
            Level = Warning
            Recommendation = "Add an <h1> tag to the <body> section of the page."
        }

    let h1Empty idx =
        {
            Category = SEO
            Code = H1Empty
            Description = "The <h1> heading summarizes the page’s topic."
            Heading = "The <h1> tag is empty."
            Index = Some idx
            Level = Warning
            Recommendation = "Add a keyword rich heading inside the <h1> tag."
        }

    let h1Multiple =
        {
            Category = Standards
            Code = H1Multiple
            Description = "The <h1> heading summarizes the page’s topic. Search engines diminish the value of a page that contains multiple <h1> tags."
            Heading = "The page contains multiple <h1> tags."
            Index = None
            Level = Warning
            Recommendation = "Use the <h1> tag once per page."
        }

    let h1Violation html =
        let h1Tags = Regex.h1.Matches html
        match h1Tags.Count with
        | 0 -> Some h1Missing
        | 1 ->
            let h1Match = h1Tags.[0]
            let h1, idx = h1Match.Groups.[1].Value, h1Match.Index
            match h1.Length with
            | 0 -> Some <| h1Empty idx
            | _ -> None
        | _ -> Some h1Multiple

    let largeInlineCss length idx =
        {
            Category = Performance
            Code = LargeInlineCss
            Description = sprintf "The page contains a large block of embedded Cascading Style Sheet (CSS) code (%d characters). Search engines will ignore CSS code, but large quantities of CSS code that precede the actual text content of the page will force the text content further down in the HTML." length
            Heading = "The page contains a large number of CSS definitions."
            Index = Some idx
            Level = Warning
            Recommendation = "Move large blocks of CSS code to externally linked CSS files."
        }

    let inlineViolations (regex:Regex) html violation =
        regex.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> x.Groups.[1].Value.Length, x.Index)
        |> List.filter (fun (length, _) -> length > 1024)
        |> List.map (fun (length, idx) -> Some <| violation length idx)

    let cssViolations html = inlineViolations Regex.style html largeInlineCss

    let largeInlineScript length idx =
        {
            Category = Performance
            Code = LargeInlineScript
            Description = sprintf "The page contains a large block of embedded script code (%d characters). Search engines will ignore script code, but large quantities of script will force the actual text content of the page further down in the HTML." length
            Heading = "The page contains a large amount of script code."
            Index = Some idx
            Level = Warning
            Recommendation = "Move all inline script code to externally linked script files."
        }

    let scriptViolations html = inlineViolations Regex.script html largeInlineScript

    let manyParamsViolation count =
        {
            Category = SEO
            Code = QueryParameterCount
            Description = sprintf "The URL contains %d query string parameters. Most search engines will analyze up to four URL parameters and ignore any additional parameters." count
            Heading = "The URL contains too many query string parameters."
            Index = None
            Level = Warning
            Recommendation = "Consider reducing the number of query string parameters, or use a URL Rewrite Module to simplify the URL structure."
        }
    
    let queryViolation (uri:Uri) =
        let paramsCount = Regex.uriParam.Matches(uri.Query).Count
        match paramsCount > 4 with
        | false -> None
        | true -> Some <| manyParamsViolation paramsCount

    let titleMissingViolation =
        {
            Category = SEO
            Code = TitleMissing
            Description = "The page markup does not contain the <title> tag. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
            Heading = "The title is missing."
            Index = None
            Level = Error
            Recommendation = "Add the <title> tag inside the <head> section of the page."
        }

    let titleEmptyViolation idx =
        {
            Category = SEO
            Code = TitleEmpty
            Description = "The <title> tag is empty. An inaccurate or irrelevant title can affect how the content of the page is indexed, ranked, and presented by search engines."
            Heading = "The title is empty."
            Index = Some idx
            Level = Error
            Recommendation = "Add a relevant title inside the <title> tag."
        }

    let titleLongViolation length idx =
        {
            Category = SEO
            Code = TitleLong
            Description = sprintf "The text in the <title> tag is too long (%d characters). The title must not exceed 65 characters, including spaces. Most search engines will truncate the text in the <title> tag after a fixed number of characters." length
            Heading = "The title is too long."
            Index = Some idx
            Level = Warning
            Recommendation = "The title must be between 5 and 65 characters long."
        }

    let titleShortViolation length idx =
        {
            Category = SEO
            Code = TitleShort
            Description = sprintf "The text in the <title> tag is too short (%d characters). The title must contain at least 5 characters. A title that is too short can affect how the content of the page is indexed, ranked, and presented by search engines." length
            Heading = "The title is too short."
            Index = Some idx
            Level = Warning
            Recommendation = "The title must be between 5 and 65 characters long."
        }

    let titleDescriptionViolation =
        {
            Category = SEO
            Code = TitleEqDescription
            Description = "The <title> and the meta description tags of the page contain identical text."
            Heading = "The title and description are identical."
            Index = None
            Level = Warning
            Recommendation = "Don't reuse the page's title in the meta description."
        }

    let titleViolations html =
        let titleMatch = Regex.title.Match html
        match titleMatch.Success with
        | false -> [Some titleMissingViolation]
        | true ->
            let title, idx = titleMatch.Groups.[1].Value, titleMatch.Index
            let lengthViolation =
                match title.Length with
                | 0 -> Some <| titleEmptyViolation idx
                | x when x < 5 -> Some <| titleShortViolation x idx
                | x when x > 65 -> Some <| titleLongViolation x idx
                | _ -> None
            let descriptionOption = Html.metaDescription html
            let contentViolation =
                match descriptionOption with
                | None -> None
                | Some description ->
                    match title.ToLower() = description.ToLower() with
                    | false -> None
                    | true -> Some titleDescriptionViolation
            [lengthViolation; contentViolation]

    let manyHyperlinksViolation count =
        {
            Category = SEO
            Code = TooManyLinks
            Description = sprintf "The page contains %d links. Too many links in the same page may affect the indexing process that is used by search engine crawlers. This may result in poor search rankings for the page or in the search engine ignoring the page." count
            Heading = "The page contains too many hyperlinks."
            Index = None
            Level = Error
            Recommendation = "Reduce the number of links on the page to less than 250."
        }

    let hyperlinksViolation html =
        let hyperlinksMatches = Regex.hyperlink.Matches html
        match hyperlinksMatches.Count with
        | x when x > 250 -> Some <| manyHyperlinksViolation x
        | _ -> None

    let refreshRedirectViolation idx =
        {
            Category = Standards
            Code = RefreshToRedirect
            Description = "The page uses a meta refresh tag to implement redirection. Using meta refresh syntax is discouraged by the W3C, because unexpected refresh may confuse users and may impair the \"back\" button functionality in some browsers."
            Heading = "The page uses a refresh definition instead of using redirection."
            Index = Some idx
            Level = Warning
            Recommendation = "Use HTTP Redirection instead of using HTTP Refresh headers or meta refresh tags."
        }
    
    let metaRefreshViolation html =
        let metaRefreshOption =
            Regex.meta.Matches html
            |> Seq.cast<Match>
            |> Seq.toList
            |> List.tryFind (fun x -> Regex.nameRefresh.IsMatch x.Value)
        match metaRefreshOption with
        | None -> None
        | Some metaRefresh -> Some <| refreshRedirectViolation metaRefresh.Index

    let rec indexPosition (lines:string []) lineIndex length index =
        let length' = lines.[lineIndex].Length + 1
        let length'' = length + length'
        match length'' > index with
        | false -> indexPosition lines (lineIndex + 1) length'' index
        | true -> lineIndex + 1, length' - (length'' - index) + 1

    let violation lines (violationPrivate:ViolationPrivate) =
        let indexPosition' = indexPosition lines 0 0
        let position =
            match violationPrivate.Index with
            | None -> None
            | Some index ->
                let line, column = indexPosition' index
                Some <| Position.New line column
        {
            Category = violationPrivate.Category
            Code = violationPrivate.Code
            Description = violationPrivate.Description
            Heading = violationPrivate.Heading
            Position = position
            Level = violationPrivate.Level
            Recommendation = violationPrivate.Recommendation
        }

/// <summary>Reviews a HTML document for markup related violations.</summary>
/// <param name="html">The HTML document.</param>
/// <param name="uri">The HTML document's URI.</param>
let review (html:string) uri =
    let lines = html.Split '\n'
    [
        yield! altViolations html
        yield metaDescriptionViolation html
        yield h1Violation html
        yield! cssViolations html
        yield! scriptViolations html
        yield queryViolation uri
        yield! titleViolations html
        yield hyperlinksViolation html
        yield metaRefreshViolation html
    ]
    |> List.choose id
    |> List.map (violation lines)