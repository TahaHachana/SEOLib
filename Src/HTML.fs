namespace SEOLib

open System.Net
open Types
open Utilities

module HTML =

    /// Removes comments, inline JS/CSS and HTML tags.
    let cleanHtml html =
        let html' = commentJSCssRegex.Replace(html, " ")
        let tagsFree  = htmlTagRegex.Replace(html', "") |> decodeHtml
        tagsFree.ToLower()
    
    /// Matches the meta tag pattern in an HTML string and
    /// returns the value and index of each successful match.
    let metaTags html =
        regexMatches metaTagRegex html
        |> Seq.toList
        |> List.map (fun x -> x.Value, x.Index)

    /// Matches the meta tag pattern in an HTML string and
    /// returns the value each successful match.
    let metaTags' html =
        regexMatches metaTagRegex html
        |> Seq.toList
        |> List.map (fun x -> x.Value)

    /// Creates a Web request and sets its timeout to 15 000 ms.
    let createWebRequest (url : string) =
        let request = WebRequest.Create url
        request.Timeout <- 15000
        request

    /// Returns the headers of a Web response.
    let headersData (response : WebResponse) =
        [
            let headers = response.Headers
            let count = headers.Count - 1
            for x in 0 .. count do
                yield headers.Keys.[x], headers.[x]
        ]

    /// Determines wether the content type of a Web response is HTML.
    let isHtml (response : WebResponse) =
        let contentType = response.ContentType
        htmlRegex.IsMatch contentType

    /// Returns the markup validation status according to the W3C validator.
    let validationStatus url =
        async {
            let url' = w3cValidatorUri + url
            let webRequest = createWebRequest url'
            let! webResponse = webRequest.AsyncGetResponse()
            let status =
                webResponse.Headers.["X-W3C-Validator-Status"] |> function
                    | "Abort"   -> Abort
                    | "Invalid" ->
                        let errors   = webResponse.Headers.["X-W3C-Validator-Errors"]
                        let warnings = webResponse.Headers.["X-W3C-Validator-Warnings"]
                        Invalid (errors, warnings)
                    | _         -> Valid
            return status
            }

    let titleTags html =
        regexMatches titleTagRegex html
        |> Seq.toList
        |> List.map (fun x -> groupValue x 1, x.Index)

    let metaDesc (metaTags : (string * int) list) =
        metaTags |> List.filter (fun (value, _) -> metaDescRegex.IsMatch value)

    let metaRefresh (metaTags : (string * int) list) =
        metaTags |> List.filter (fun (value, _) -> metaRefreshRegex.IsMatch value)

    let metaDescContent metaTag =
        let contentMatch = metaDescContRegex.Match(metaTag)
        groupValue contentMatch 2

    let h1Tags html = regexMatches h1TagRegex html