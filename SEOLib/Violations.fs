namespace SEOLib

open System
open Utilities

module Violations =

    /// <summary>Returns violations related to the "alt" attribute in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The "alt" attribute related warnings.</returns>
    let checkAltAttributes html =
        async {
            return
                regexMatches imgTagRegex html
                |> Seq.toArray
                |> Array.map altAttribute
                |> Array.partition (fun (_, _, alt) -> alt.IsSome)
                |> function
                    | someAlt, noneAlt ->
                        let someAlt' =
                            someAlt
                            |> Array.filter (fun (_, _, alt) -> alt.Value.Length  = 0)
                            |> Array.map (fun (_, index, _) -> makeAltEmptyViolation <| Some index)
                        let noneAlt' = noneAlt |> Array.map (fun (_, index, _) ->
                            makeAltMissingViolation <| Some index)
                        Array.append someAlt' noneAlt'
        }

    /// <summary>Returns violations related to the meta description element in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The meta description related warnings.</returns>
    let checkMetaDescription html =
        async {
            let tags = metaDescTags html
            let count = tags.Length
            return
                match count with
                    | _ when count > 1 -> [| makeDescriptionMultipleViolation count None |]
                    | _ when count = 0 -> [| makeDescriptionMissingViolation None |]
                    | _ ->
                        let metaDesc, index = tags.[0]
                        let index' = Some index
                        let attributes = tagAttributes metaDesc
                        let contentAttrOption = attributes |> Array.tryFind (fun x -> contentAttrRegex.IsMatch x.Key)
                        match contentAttrOption with
                            | None -> [| makeDescriptionEmptyViolation index' |]
                            | Some contentAttr ->
                                let length = contentAttr.Value.Length
                                match length with
                                    | _ when length = 0   -> [| makeDescriptionEmptyViolation None |]
                                    | _ when length > 150 -> [| makeDescriptionLongViolation length index' |]
                                    | _ when length < 25 -> [| makeDescriptionShortViolation length index' |]
                                    | _ -> [||]
            }

    /// <summary>Returns violations related to the heading of a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The heading related warnings.</returns>
    let checkH1 html =
        async {
            let h1s = h1Tags html
            let count = Seq.length h1s
            return
                match count with
                    | 0 -> [| makeH1MissingViolation None |]
                    | 1 ->
                        let h1, index = h1s |> Seq.nth 0 |> (fun x -> x.Value, x.Index)
                        let h1Length = h1.Length
                        match h1Length with
                            | 0 -> [| makeH1EmptyViolation <| Some index |]
                            | _ -> [||]
                    | _ -> [| makeH1MultipleViolation None |]
        }

    /// <summary>Returns violations related to inline CSS in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The CSS related warnings.</returns>
    let checkInlineCss html =
        async {
            return
                f inlineCssRegex html makeLargeInlineCssViolation
        }

    /// <summary>Returns violations related to inline JavaScript in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The JavaScript related warnings.</returns>
    let checkInlineScript html =
        async {
            return
                f inlineScriptRegex html makeLargeInlineScriptViolation
            }

    /// <summary>Returns violations related to the query in a URI.</summary>
    /// <param name="uri">The URI to check.</param>
    /// <returns>The URI query related warnings.</returns>
    let checkQueryParameters (uri : Uri) =
        async {
            let query = uri.Query
            let paramsCount = regexMatches queryParamRegex query |> Seq.length
            return
                match paramsCount with
                    | _ when paramsCount > 4 -> [| makeQueryParameterCountViolation paramsCount |]
                    | _                      -> [||]
        }

    /// <summary>Returns violations related to the title of a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The title related errors and warnings.</returns>
    let checkTitle html =
        async {
            let titleOption = title html
            return
                match titleOption with
                    | None -> [| makeTitleMissingViolation None |]
                    | Some (titleOption', index) ->
                        let index' = Some index
                        match titleOption' with
                            | None -> [| makeTitleEmptyViolation index' |]
                            | Some title ->
                                let violations =
                                    let titleLength = title.Length
                                    match titleLength with
                                        | _ when titleLength > 65 ->
                                            [| makeTitleLongViolation titleLength index' |]
                                        | _ when titleLength < 5 ->
                                            [| makeTitleShortViolation titleLength index' |]
                                        | _ -> [||]
                                let violations' =
                                    let metas = Html.metaTags html
                                    let metaDescOption = Html.metaDescription metas
                                    match metaDescOption with
                                        | None -> [||]
                                        | Some metaDesc ->
                                            let titleEqualsDesc = title.ToLower() = metaDesc.ToLower()
                                            match titleEqualsDesc with
                                                | false -> [||]
                                                | true  -> [| makeTitleEqDescriptionViolation None |]
                                Array.append violations violations'
        }

    /// <summary>Checks the number of links in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>A too many links violation if their count exceeds 250.</returns>
    let checkLinks html =
        async {
            let aTagsCount = regexMatches aTagRegex html |> Seq.length
            return
                match aTagsCount with
                    | _ when aTagsCount > 250 -> [| makeTooManyLinksViolation aTagsCount |]
                    | _                       -> [||]
            }

    /// <summary>Checks if the meta refresh tag is used in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>A refresh to redirect violation if the meta refresh element is used.</returns>
    let checkMetaRefresh html =
        async {
            let metaRefreshOption = metaRefreshTag html
            return
                match metaRefreshOption with
                    | None -> [||]
                    | Some (_, index) -> [| makeRefreshRedirectViolation <| Some index |]
            }

    /// <summary>Returns violations found in a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <param name="uri">The Web request URI.</param>
    /// <returns>The detected violations.</returns>
    let auditHtml html uri =
        [
            checkAltAttributes   html
            checkMetaDescription html
            checkH1              html
            checkInlineCss       html
            checkInlineScript    html
            checkQueryParameters uri
            checkTitle           html
            checkLinks           html
            checkMetaRefresh     html
        ] |> Async.Parallel