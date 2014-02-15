module SEOLib.Html

open System
open System.Net
open System.Text.RegularExpressions

let private groupOption (regexMatch:Match) (group:int) =
    match regexMatch.Groups.[group].Value.Trim() with
    | "" -> None
    | title -> Some <| WebUtility.HtmlDecode title

/// <summary>Returns the title of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let title html =
    let titleMatch = Regex("(?is)<title>(.+?)</title>").Match html
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

type MetaAttr = HttpEquiv of string | Name of string 

let private httpEquivRegex = Regex("(?i)http-equiv=(\"|')([^\"']+)(\"|')", RegexOptions.Compiled)    
let private nameRegex = Regex("(?i)name=(\"|')([^\"']+)(\"|')", RegexOptions.Compiled)
let private contentRegex = Regex("(?i)content=(\"|')([^\"']+)(\"|')", RegexOptions.Compiled)
    
let private metaAttr input =
    let httpEquivMatch = httpEquivRegex.Match input
    match httpEquivMatch.Success with
    | false ->
        let nameMatch = nameRegex.Match input
        match nameMatch.Success with
        | false -> None
        | true -> nameMatch.Groups.[2].Value.Trim() |> Name |> Some
    | true -> httpEquivMatch.Groups.[2].Value.Trim() |> HttpEquiv |> Some

let private metaContent input =
    let contentMatch = contentRegex.Match input
    match contentMatch.Success with
    | false -> None
    | true -> groupOption contentMatch 2

let private httpEquivName metaAttrOption =
    match metaAttrOption with
    | None -> None, None
    | Some x ->
        match x with
        | HttpEquiv x -> Some x, None
        | Name x -> None, Some x

/// <summary>Returns the meta elements of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let meta html =
    Regex("(?i)<meta .+?>").Matches html
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun x ->
        let metaValue = x.Value
        let httpEquiv, name = httpEquivName <| metaAttr metaValue
        Meta.New httpEquiv name <| metaContent metaValue)

let private metaElt html pattern =
    Regex("(?i)<meta .+?>").Matches html
    |> Seq.cast<Match>
    |> Seq.tryFind (fun x -> Regex(pattern).IsMatch x.Value)
    |> function
    | None -> None
    | Some x -> metaContent x.Value

/// <summary>Returns the meta description of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let metaDescription html = metaElt html "(?i)name=(\"|')description(\"|')"

/// <summary>Returns the meta keywords of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let metaKeywords html = metaElt html "(?i)name=(\"|')keywords(\"|')"

let private stripHtml html =
    Regex("(?is)(<!--.*?--\s*>|<script.*?</script>|<style.*?</style>)").Replace(html, "")
    |> fun x -> Regex("<.+?>").Replace(x, "").Trim()

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
    Regex("(?is)<h([1-6])>(.+?)</h[1-6]>").Matches html
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun x ->
        let level = x.Groups.[1].Value |> int
        let text =
            x.Groups.[2].Value
            |> stripHtml
            |> WebUtility.HtmlDecode
        Heading.New level text)

let private stripSpaces html =
    Regex("(\n|\r)").Replace(html, " ")
    |> fun x -> Regex(" {2,}").Replace(x, " ")

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