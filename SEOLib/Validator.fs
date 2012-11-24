namespace SEOLib

open System
open Utilities

module Validator =

    /// <summary>Checks if a URI is valid. This is a quick validation way because
    /// only the HTTP response is parsed.</summary>
    /// <param name="uriString">The URI to validate.</param>
    /// <returns>The validty status of the specified URI.</returns>
    let isValid uriString =
        async {
            try
                let uriString' = Uri(buildUri uriString)
                let client = httpClient ()
                let! httpResponse = awaitHttpResponse client uriString'
                let status =
                    httpResponse.Headers.GetValues "X-W3C-Validator-Status"
                    |> Seq.nth 0
                    |> function
                        | "Invalid" -> Invalid
                        | _         -> Valid
                    |> Some
                return status
            with _ -> return None
        }

    /// <summary>Validates the markup of a HTML page.</summary>
    /// <param name="uriString">The URI to validate.</param>
    /// <returns>A MarkupValidation type describing the validty status of the specified URI.</returns>
    let validateUri uriString =
        let uriString' = buildUri uriString
        let xdocumentOption = loadXdocument uriString'
        match xdocumentOption with
            | None -> None
            | Some xdocument ->
                let descendants = xdocument.Descendants ()
                let findByLocalname' = findByLocalname descendants
                let collectMarkupErrors' = collectMarkupErrors descendants
                let doctype = findByLocalname' "doctype"
                let charset = findByLocalname' "charset"
                let validity =
                    findByLocalname' "validity"
                    |> function "false" -> Invalid | _ -> Valid
                let errorCount = findByLocalname' "errorcount" |> int
                let warningCount = findByLocalname' "warningcount" |> int
                let errors = collectMarkupErrors' "error" errorCount
                let warnings = collectMarkupErrors' "warning" warningCount
                makeMarkupValidation doctype charset validity errorCount warningCount errors warnings
                |> Some