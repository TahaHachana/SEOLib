namespace SEOLib

open System
open Utilities

module Links =
    
    /// <summary>Collects links from a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <param name="requestUri">The Web request Uri.</param>
    /// <returns>The links list.</returns>
    let collectLinks html requestUri =
        let baseHref = baseUri html requestUri
        let html' = removeComments html
        let absoluteUris, relativeUris = collectHrefs html' |> partitionLinks
        let absoluteUris' =
            absoluteUris
            |> List.map (fun (href, anchor, follow) ->
                let isUri, result = Uri.TryCreate(href, UriKind.Absolute)
                isUri, result, anchor, follow)
            |> List.filter (fun (isUri, result, anchor, follow) -> isUri = true)
            |> List.map (fun (isUri, result, anchor, follow) -> result, anchor, follow)
        let relativeUris' = formatRelativeUris relativeUris requestUri baseHref
        absoluteUris'
        |> List.append relativeUris'
        |> Seq.distinctBy (fun (x, _, _) -> x)
        |> Seq.toList
        |> makeLink' requestUri


//
//open System
//open System.IO
//open System.Net
//open System.Net.Http
//open System.Net.Http.Headers
//open System.Text.RegularExpressions
//open Html
//open Types
//open Utilities
//open Http
//
//module Links =
//
//    let private relAttributeContent anchor =
//        relAttributeRegex.Match(anchor).Groups.[2].Value
//        |> (fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
//        |> List.ofArray
//        |> List.map (fun x -> x.Trim())
//
//    /// Matches <a> tags in an HTML string.
//    let private scrapeUris html =
//        anchorRegex.Matches html
//        |> Seq.cast<Match>
//        |> Seq.toList
//        |> List.map (fun x -> x.Value)
//        |> List.map (fun x -> x, relAttributeContent x)
//        |> List.map (fun (anchor, rel) ->
//            let href = hrefRegex.Match(anchor).Groups.[2].Value
//            let follow =
//                rel
//                |> List.tryFind (fun x -> noFollowRegex.IsMatch x)
//                |> function Some _ -> NoFollow | None -> DoFollow
//            href, follow)
//
//    let private partitionUris uris = uris |> List.partition (fun (href, follow) -> absoluteUriRegex.IsMatch href)
//
//    let private formatRelativeUris relativeUris host =
//        relativeUris
//        |> List.map (fun (href, follow) ->
//            let href' =
//                let m = Regex("^/").IsMatch href
//                match m with
//                    | true -> Regex("^/").Replace(href, "") |> (fun x -> "http://" + x)
//                    | false -> "http://" + host + "/" + href
//            href', follow)
//
//    /// Scrapes links from an HTML string optionally retaining only internal ones.
//    let private scrapeLinks host html onlyInternal =
//        let absoluteUris, relativeUris = scrapeUris html |> partitionUris
//        let relativeUris' = formatRelativeUris relativeUris host
//        let allLinks =
//            absoluteUris
//            |> List.append relativeUris'
//            |> List.map (fun (href, follow)  -> Uri(href).ToString(), follow)
//            |> Seq.distinctBy fst
//            |> Seq.toList
//        match onlyInternal with
//            | false -> allLinks
//            | true  ->
//                let hostRegex = compileRegex host
//                allLinks |> List.filter (fun (href, _) -> hostRegex.IsMatch href)
//
//    /// Collects links from the given Web page.
//    let collectLinks host (webPage : WebPage) onlyInternal =
//        let html = webPage.Html
//        scrapeLinks host html onlyInternal