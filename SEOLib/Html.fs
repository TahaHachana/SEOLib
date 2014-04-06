module SEOLib.Html

open System
open System.Net
open System.Text.RegularExpressions

type MetaAttr =
    | HttpEquiv of string
    | Name of string 

type Hyperlink =
    {
        Uri : string
        Anchor : string
        Type : LinkType
        Rel : Rel
    }

    static member New uri anchor linkType rel =
        {
            Uri = uri
            Anchor = anchor
            Type = linkType
            Rel = rel
        }

and LinkType = External | Internal

and Rel = Follow | NoFollow

[<AutoOpen>]
module private Utils =

    let groupOption (regexMatch:Match) (group:int) =
        match regexMatch.Groups.[group].Value.Trim() with
        | "" -> None
        | x -> Some <| WebUtility.HtmlDecode x

    let metaAttr input =
        let httpEquivMatch = Regex.httpEquivAttr.Match input
        match httpEquivMatch.Success with
        | false ->
            let nameMatch = Regex.nameAttr.Match input
            match nameMatch.Success with
            | false -> None
            | true ->
                nameMatch.Groups.[2].Value.Trim()
                |> Name
                |> Some
        | true ->
            httpEquivMatch.Groups.[2].Value.Trim()
            |> HttpEquiv
            |> Some

    let metaContent input =
        let contentMatch = Regex.contentAttr.Match input
        match contentMatch.Success with
        | false -> None
        | true -> groupOption contentMatch 2

    let httpEquivName metaAttrOption =
        match metaAttrOption with
        | None -> None, None
        | Some x ->
            match x with
            | HttpEquiv x -> Some x, None
            | Name x -> None, Some x

    let metaElt html pattern =
        Regex.meta.Matches html
        |> Seq.cast<Match>
        |> Seq.tryFind (fun x -> Regex(pattern).IsMatch x.Value)
        |> function
        | None -> None
        | Some x -> metaContent x.Value

    let stripHtml html =
        Regex.commentScriptCss.Replace(html, "")
        |> fun x -> Regex.tag.Replace(x, "").Trim()

    let stripSpaces html =
        Regex.newline.Replace(html, " ")
        |> fun x -> Regex.space.Replace(x, " ")

    let baseUri html requestUri =
        let baseMatch = Regex.``base``.Match html
        match baseMatch.Success with
        | false -> requestUri
        | true ->
            let hrefMatch = Regex.href.Match baseMatch.Value
            match hrefMatch.Success with
            | false -> requestUri
            | true ->
                let href = hrefMatch.Groups.[2].Value
                match Uri.TryCreate(href, UriKind.Absolute) with
                | false, _ -> requestUri
                | true, x -> x

    let anchor (linkMatch:Match) =
        linkMatch.Groups.[2].Value
        |> fun x -> Regex.scriptCss.Replace(x, "")
        |> fun x -> Regex.tag.Replace(x, "").Trim()
        |> WebUtility.HtmlDecode

    let relAttributes tag =
        Regex.relAttr.Match(tag).Groups.[2].Value
        |> (fun x -> x.Split([|' '|], StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray

    let hrefs html =
        Regex.hyperlink.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun linkMatch ->
            let openTag = linkMatch.Groups.[1].Value
            let href = Regex.href.Match(openTag).Groups.[2].Value
            let linkAnchor = anchor linkMatch
            let follow =
                relAttributes openTag
                |> List.tryFind (fun x -> Regex("(?i)nofollow").IsMatch x)
                |> function None -> Follow | _ -> NoFollow
            href, linkAnchor, follow)

    let toAbsolute relativeLinks baseUri =
        [
            for (href:string, anchor, follow) in relativeLinks do
                let isUri, result = Uri.TryCreate(baseUri, href)
                match isUri with
                | false -> ()
                | true -> yield result, anchor, follow
        ]

    let makeHyperlinks (uri:Uri) links =
        let host = uri.Host
        links |> List.map
            (fun (uri:Uri, anchor, follow) ->
                let isInternal = uri.Host = host
                let uriString = uri.ToString()
                match isInternal with
                | false -> Hyperlink.New uriString anchor External follow
                | true -> Hyperlink.New uriString anchor Internal follow)

/// <summary>Returns the title of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let title html =
    let titleMatch = Regex.title.Match html
    match titleMatch.Success with
    | false -> None
    | true -> groupOption titleMatch 1

type Meta =
    {
        HttpEquiv : string option
        Name : string option
        Content : string option
    }
        
    static member New httpEquiv name content =
        {
            HttpEquiv = httpEquiv
            Name = name
            Content = content
        }

/// <summary>Returns the meta elements of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let meta html =
    Regex.meta.Matches html
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun x ->
        let metaValue = x.Value
        let httpEquiv, name = httpEquivName <| metaAttr metaValue
        Meta.New httpEquiv name <| metaContent metaValue)

/// <summary>Returns the meta description of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let metaDescription html = metaElt html "(?i)name=(\"|')description(\"|')"

/// <summary>Returns the meta keywords of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let metaKeywords html = metaElt html "(?i)name=(\"|')keywords(\"|')"

type Heading =
    {
        Level : int
        Text : string
    }

    static member New level text =
        {
            Level = level
            Text = text
        }

/// <summary>Returns the headings of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let headings html =
    Regex.heading.Matches html
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun x ->
        let text =
            x.Groups.[2].Value
            |> stripHtml
            |> WebUtility.HtmlDecode
        Heading.New (int x.Groups.[1].Value) text)

/// <summary>Calculates the text/HTML ratio in a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let textHtmlRatio html =
    let textLength =
        stripHtml html
        |> stripSpaces
        |> WebUtility.HtmlDecode
        |> fun x -> float x.Length
    textLength / (float html.Length) * 100.
    |> fun x -> Math.Round(x, 2)

/// <summary>Returns the hyperlinks of HTML document.</summary>
/// <param name="html">The HTML document.</param>
/// <param name="uri">The HTML document's URI.</param>
let hyperlinks html uri =
    let html' = Regex.comment.Replace(html, "*")
    let baseUri = baseUri html' uri
    let absolute, relative =
        hrefs html' |> List.partition (fun (href, _, _) ->
            Regex.uri.IsMatch href)
    let absolute' =
        absolute
        |> List.map (fun (href, anchor, rel) ->
            let isUri, result = Uri.TryCreate(href, UriKind.Absolute)
            isUri, result, anchor, rel)
        |> List.filter (fun (isUri, _, _, _) -> isUri)
        |> List.map (fun (isUri, result, anchor, follow) ->
            result, anchor, follow)
    absolute'
    |> List.append <| toAbsolute relative baseUri
    |> Seq.distinctBy (fun (x, _, _) -> x)
    |> Seq.toList
    |> makeHyperlinks uri