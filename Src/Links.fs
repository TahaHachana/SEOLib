module Links

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open Helpers
open Types
open HTML

/// Filters X-Robots-Tag headers.
let filterXRobotsTag headers =
    headers
    |> List.filter (fun x -> fst x = "X-Robots-Tag")
    |> function
        | [] -> []
        | lst -> lst |> List.map snd

let tryFindMetaRobots html =
    metaPatternRegex.Match(html).Value |> function
    | "" -> None
    | x -> robotsPatternRegex.Match(x).Value |> function
        | "" -> None
        | _ -> Some x

/// Process X-Robots-Tag headers.
// If multiple X-Robots-Tag directives are combined
// we keep only those specific to googlebot.
// If googlebot is not specified we keep only the
// directives that don't include a user-agent.
let xRobotsTagData headers =
    let length = List.length headers
    match length with
        | 0 -> None
        | 1 -> Some headers
        | _ ->
            headers
            |> List.filter (fun x -> googleBotRegex.IsMatch x)
            |> function
                | [] ->
                    headers
                    |> List.filter (fun x -> userAgentRegex.IsMatch x = false)
                    |> function
                        | [] -> None
                        | lst -> Some lst
                | lst -> Some lst

/// Determines wether a robots directives list contains a regex match.
let matchesPattern lst (directiveRegex : Regex) =
    lst
    |> List.tryFind directiveRegex.IsMatch
    |> function None -> false | Some _ -> true

let constructWebPage responseUri html headers isNoFollow =
    Some
        {
            ResponseUri = responseUri
            HTML = html
            Headers = headers
            NoFollow = isNoFollow
        }

/// Reads a Web response and returns a WebPage.
let readWebResponse (response : WebResponse) lst headers isNoIndex =
    try
        let responseUri = response.ResponseUri |> Some
        use stream = response.GetResponseStream()
        use reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        let robotsMeta = tryFindMetaRobots html
        match robotsMeta with
            | Some meta ->
                let isNoIndex = matchesPattern [meta] noIndexRegex
                let isNoFollow = matchesPattern [meta] noFollowRegex
                match isNoIndex with
                    | true ->
                        constructWebPage None html headers isNoFollow
                    | false ->
                        constructWebPage responseUri html headers isNoFollow
            | None ->
                let responseUri' = isNoIndex |> function true -> None | false -> responseUri 
                let isNoFollow = matchesPattern lst noFollowRegex
                constructWebPage responseUri' html headers isNoFollow
    with _ -> None

/// Returns a WebPage option for a Web response.
let webPage directives response headers =
    match isHtml response with
        | false -> None
        | true  ->
            matchesPattern directives noneRegex |> function
                | true -> None
                | false ->
                    matchesPattern directives noIndexRegex |> function
                        | true -> readWebResponse response directives headers true
                        | false -> readWebResponse response directives headers false

/// Fetches the content of a url.
let fetchUrl url =
    let request = createWebRequest url
    let response = request.GetResponse()
    let headers = headersData response
    filterXRobotsTag headers
    |> xRobotsTagData
    |> function
        | Some lst -> webPage lst response headers
        | None -> webPage [] response headers

/// Matches anchor tags in an HTML string.
let anchors html =
    anchorRegex.Matches html
    |> Seq.cast<Match>
    |> Seq.toList
    |> List.map (fun x -> x.Value)
    |> List.filter (fun x -> noFollowRegex.IsMatch x = false)

/// Matches the values of the href attribute in anchor tags.
let hrefs anchors =
    anchors
    |> List.map (fun x -> hrefRegex.Match(x).Groups.[2].Value)
    |> List.filter (fun x -> x <> "")

let scrapeUris = anchors >> hrefs

let partitionUris uris = uris |> List.partition (fun x -> absoluteUriRegex.IsMatch x)

let formatRelativeUris relativeUris host =
    relativeUris
    |> List.map (fun x ->
        let m = Regex("^/").IsMatch x
        match m with
            | true -> Regex("^/").Replace(x, "") |> (fun x -> "http://" + x)
            | false -> "http://" + host + "/" + x)

/// Scrapes links from an HTML string optionally filtering internal ones.
let scrapeLinks host html onlyInternal =
    let absoluteUris, relativeUris =
        html
        |> scrapeUris
        |> partitionUris
    let relativeUris' = formatRelativeUris relativeUris host
    let allLinks =
        absoluteUris
        |> List.append relativeUris'
        |> List.map (fun x -> Uri(x).ToString())
        |> Seq.distinct
        |> Seq.toList
    match onlyInternal with
        | false -> allLinks
        | true  ->
            let regex = compileRegex host
            allLinks |> List.filter regex.IsMatch

/// Collects links from the given Web page.
let collectLinks onlyInternal url host =
    let webPage' = fetchUrl url
    match webPage' with
        | Some data ->
            let isNoFollow = data.NoFollow
            match isNoFollow with
                | true -> []
                | false ->
                    let html = data.HTML
                    scrapeLinks host html onlyInternal
        | None -> []