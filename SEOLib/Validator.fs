namespace SEOLib

#if INTERACTIVE
#r "System.Xml.Linq"
#endif

open System.Xml.Linq
open System.Web
open Types
open Utilities

module Validator =

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

