namespace SEOLib

open System.Text.RegularExpressions
open Types
open Utilities

module Html =

    /// <summary>Returns the title of a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The content of the title element.</returns>
    let title html =
        sndGroupValue titleRegex html
        |> checkEmptyString'

    /// <summary>Returns the meta elements of a HTML document as a string array.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The meta elements.</returns>
    let metaTags html =
        metaTagRegex.Matches html
        |> Seq.cast<Match>
        |> Seq.toArray
        |> Array.map fstGroupValue'

    /// <summary>Returns the content of the meta description element.</summary>
    /// <param name="metaTags">The meta elements as a string array.</param>
    /// <returns>The meta description content.</returns>
    let metaDescription metaTags =
        metaTag metaTags metaDescRegex contentAttrRegex

    /// <summary>Returns the content of the meta keywords element.</summary>
    /// <param name="metaTags">The meta elements as a string array.</param>
    /// <returns>The meta keywords content.</returns>
    let metaKeywords metaTags =
        metaTag metaTags metaKeysRegex contentAttrRegex

    /// <summary>Returns the headings of a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The headings of the document.</returns>    
    let headings html =
        headingRegex.Matches html
        |> Seq.cast<Match>
        |> Seq.toList
        |> List.map (fun x ->
            makeHeading (sndGroupValue' x) (trdGroupValue' x))