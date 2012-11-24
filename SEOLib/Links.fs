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