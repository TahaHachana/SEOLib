module HTML

open System.Net
open Helpers
open Types

/// Removes comments, inline JS/CSS and HTML tags.
let cleanHtml html =
    let pattern  = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
    let pattern' = "(?i)<[^>]*>"
    let regex  = compileRegex pattern
    let regex' = compileRegex pattern'
    let html' = regex.Replace(html, " ")
    let tagsFree  = regex'.Replace(html', "") |> WebUtility.HtmlDecode
    tagsFree.ToLower()

let metaTags html =
    let metaRegex = compileRegex "(?is)<meta\ .+?>"
    regexMatches metaRegex html
    |> Seq.toList
    |> List.map (fun x -> x.Value, x.Index)

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
        let url' = "http://validator.w3.org/check?uri=" + url // + "&output=soap12"
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
    let titleRegex = compileRegex "(?is)<title>(.+?)</title>"
    regexMatches titleRegex html
    |> Seq.toList
    |> List.map (fun x -> x.Groups.[1].Value, x.Index)

let metaDesc (metaTags : (string * int) list) =
    let metaDescRegex = compileRegex "name=(\"|')description(\"|')"
    metaTags |> List.filter (fun (value, _) -> metaDescRegex.IsMatch value)

let metaRefresh (metaTags : (string * int) list) =
    let metaDescRegex = compileRegex "http-equiv=(\"|')refresh(\"|')"
    metaTags |> List.filter (fun (value, _) -> metaDescRegex.IsMatch value)

let metaDescContent metaTag =
    let contentRegex = compileRegex "content=(\"|')(.+?)(\"|')"
    contentRegex.Match(metaTag).Groups.[2].Value

let h1Tags html =
    let h1Regex = compileRegex "(?is)<h1>(.+?)</h1>"
    regexMatches h1Regex html

