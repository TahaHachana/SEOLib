namespace SEOLib

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text.RegularExpressions
open Html
open Types
open Utilities
open Http

module Links =

    let private relAttributeContent anchor =
        relAttributeRegex.Match(anchor).Groups.[2].Value
        |> (fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun x -> x.Trim())

    /// Matches <a> tags in an HTML string.
    let private scrapeUris html =
        anchorRegex.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x -> x.Value)
        |> List.map (fun x -> x, relAttributeContent x)
        |> List.map (fun (anchor, rel) ->
            let href = hrefRegex.Match(anchor).Groups.[2].Value
            let follow =
                rel
                |> List.tryFind (fun x -> noFollowRegex.IsMatch x)
                |> function Some _ -> NoFollow | None -> DoFollow
            href, follow)

    let private partitionUris uris = uris |> List.partition (fun (href, follow) -> absoluteUriRegex.IsMatch href)

    let private formatRelativeUris relativeUris host =
        relativeUris
        |> List.map (fun (href, follow) ->
            let href' =
                let m = Regex("^/").IsMatch href
                match m with
                    | true -> Regex("^/").Replace(href, "") |> (fun x -> "http://" + x)
                    | false -> "http://" + host + "/" + href
            href', follow)

    /// Scrapes links from an HTML string optionally retaining only internal ones.
    let private scrapeLinks host html onlyInternal =
        let absoluteUris, relativeUris = scrapeUris html |> partitionUris
        let relativeUris' = formatRelativeUris relativeUris host
        let allLinks =
            absoluteUris
            |> List.append relativeUris'
            |> List.map (fun (href, follow)  -> Uri(href).ToString(), follow)
            |> Seq.distinctBy fst
            |> Seq.toList
        match onlyInternal with
            | false -> allLinks
            | true  ->
                let hostRegex = compileRegex host
                allLinks |> List.filter (fun (href, _) -> hostRegex.IsMatch href)

    /// Collects links from the given Web page.
    let collectLinks host (webPage : WebPage) onlyInternal =
        let html = webPage.Html
        scrapeLinks host html onlyInternal