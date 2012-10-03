namespace SEOLib

open System.Net
open System.Text.RegularExpressions

module HTML =

    ///Compiles a pattern into a Regex object.
    let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

    /// Extracts the specified tag from a HTML string.
    let tag tagName html =
        let pattern = String.concat "" ["(?is)<"; tagName; ".*?/?>.+?</"; tagName; ">"]
        let regex = compileRegex pattern
        regex.Match(html).Value |> function
            | "" -> None
            | x  -> Some x

    /// Extracts the specified tag array from a HTML string.
    let tagArray tagName html =
        let pattern = String.concat "" ["(?is)<"; tagName; ".*?/?>.+?(</"; tagName; ">)?"]
        let regex = compileRegex pattern
        regex.Matches(html)
        |> Seq.cast<Match>
        |> Seq.toArray
        |> Array.map (fun x -> x.Value)
        |> function
            | [||] -> None
            | x  -> Some x

    let htmlTagPattern = "(?i)<[^>]*>"
    let htmlTagRegex = compileRegex htmlTagPattern

    /// Decodes HTML encoded characters.
    let decodeHtml html = WebUtility.HtmlDecode html

    /// Removes HTML tags and decodes encoded characters.
    let stripTags =
        function
            | None -> None
            | Some str ->
                htmlTagRegex.Replace(str, "").Trim() |> function
                    | "" -> None
                    | x  -> Some <| decodeHtml x

    let attribute attributeName html =
        let pattern = String.concat "" ["(?is)"; attributeName; "=('|\")(.*?)('|\")"]
        let regex = compileRegex pattern
        regex.Match(html).Groups.[2].Value |> function
            | "" -> None
            | x  -> Some <| decodeHtml x

    let metaDescriptionPattern = "(?i)name=('|\")description('|\")"
    let metaDescriptionRegex = compileRegex metaDescriptionPattern

    let metaKeywordsPattern = "(?i)name=('|\")keywords('|\")"
    let metaKeywordsRegex = compileRegex metaKeywordsPattern

    /// <summary>Returns the title of a Web page.</summary>
    /// <param name="html">The source of the Web page</param>
    /// <returns>The page title if it exists.</returns>
    let title html = tag "title" html |> stripTags


    /// <summary>Returns the meta description content of a Web page.</summary>
    /// <param name="metaTagsOption">A string array option containing the meta tags of the page.</param>
    /// <returns>The page meta description content if it exists.</returns>
    let metaDescription metaTagsOption =
        match metaTagsOption with
            | None -> None
            | Some metaTags ->
                Array.tryFind (fun x -> metaDescriptionRegex.IsMatch x) metaTags |> function
                    | None -> None
                    | Some x -> attribute "content" x

    /// <summary>Returns the meta keywords content of a Web page.</summary>
    /// <param name="metaTagsOption">A string array option containing the meta tags of the page.</param>
    /// <returns>The page meta keywords content if it exists.</returns>    
    let metaKeywords metaTagsOption =
        match metaTagsOption with
            | None -> None
            | Some metaTags ->
                Array.tryFind (fun x -> metaKeywordsRegex.IsMatch x) metaTags |> function
                    | None -> None
                    | Some x -> attribute "content" x

    /// <summary>Returns the heading (H1) of a Web page.</summary>
    /// <param name="html">The source of the page.</param>
    /// <returns>The page heading if it exists.</returns>    
    let heading html = tag "h1" html

    // ================================================================================================
    let html =
        use client = new WebClient()
        client.DownloadString "http://www.oracle.com/"
    
    let titleOption = title html
    let metaTagsOption = tagArray "meta" html
    let descriptionOption = metaDescription metaTagsOption
    let keywordsOption = metaKeywords metaTagsOption
    let headingOption = heading html

    // ================================================================================================



//    let commentJSCssPattern  = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
//    let commentJSCssRegex  = compileRegex commentJSCssPattern
//    
//    /// Removes comments, inline JavaScript and CSS from HTML.
//    let stripCommentsJSCSS html = commentJSCssRegex.Replace(html, " ")
//
//
//    /// Decodes HTML encoded characters.
//    let decodeHtml html = WebUtility.HtmlDecode html
//
    

    
//open System.Net
//open Types
//open Utilities
//
//module Html =
//
//    /// Removes comments, inline JS/CSS and HTML tags.
//    let cleanHtml html =
//        let html' = commentJSCssRegex.Replace(html, " ")
//        let tagsFree  = htmlTagRegex.Replace(html', "") |> decodeHtml
//        tagsFree.ToLower()
//    
//    /// Matches the meta tag pattern in an HTML string and
//    /// returns the value and index of each successful match.
//    let metaTags html =
//        regexMatches metaTagRegex html
//        |> Seq.toList
//        |> List.map (fun x -> x.Value, x.Index)
//
//    /// Matches the meta tag pattern in an HTML string and
//    /// returns the value of each successful match.
//    let metaTags' html =
//        regexMatches metaTagRegex html
//        |> Seq.toList
//        |> List.map (fun x -> x.Value)
//
//    /// Creates a Web request and sets its timeout to 15 000 ms.
//    let createWebRequest (url : string) =
//        let request = WebRequest.Create url
//        request.Timeout <- 15000
//        request
//
//    /// Returns the headers of a Web response.
//    let headersData (response : WebResponse) =
//        [
//            let headers = response.Headers
//            let count = headers.Count - 1
//            for x in 0 .. count do
//                yield headers.Keys.[x], headers.[x]
//        ]
//
//    /// Determines wether the content type of a Web response is HTML.
//    let isHtml (response : WebResponse) =
//        let contentType = response.ContentType
//        htmlRegex.IsMatch contentType
//
//    /// Returns the markup validation status according to the W3C validator.
//    let validationStatus url =
//        async {
//            let url' = w3cValidatorUri + url
//            let webRequest = createWebRequest url'
//            let! webResponse = webRequest.AsyncGetResponse()
//            let status =
//                webResponse.Headers.["X-W3C-Validator-Status"] |> function
//                    | "Abort"   -> Abort
//                    | "Invalid" ->
//                        let errors   = webResponse.Headers.["X-W3C-Validator-Errors"]
//                        let warnings = webResponse.Headers.["X-W3C-Validator-Warnings"]
//                        Invalid (errors, warnings)
//                    | _         -> Valid
//            return status
//            }
//
//    /// Matches the title tag pattern in HTML code.
//    let titleTags html =
//        regexMatches titleTagRegex html
//        |> Seq.toList
//        |> List.map (fun x -> groupValue x 1, x.Index)
//
//    let metaDesc (metaTags : (string * int) list) =
//        metaTags |> List.filter (fun (value, _) -> metaDescRegex.IsMatch value)
//
//    let metaRefresh (metaTags : (string * int) list) =
//        metaTags |> List.filter (fun (value, _) -> metaRefreshRegex.IsMatch value)
//
//    let metaDescContent metaTag =
//        let contentMatch = metaDescContRegex.Match(metaTag)
//        groupValue contentMatch 2
//
//    let h1Tags html = regexMatches h1TagRegex html