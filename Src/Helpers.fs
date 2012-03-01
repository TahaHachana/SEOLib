module Helpers

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open Types

/// Compiles a regex.
let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

/// Reads all the lines of a text file and creates a list from the resulting array.
let readAllLines textFile = File.ReadAllLines textFile |> Array.toList

/// Rounds the third element of a tuple.
let roundThird (x, y, z : float) = x, y, Math.Round(z, 2)

/// Returns the value of a Nullable<int> or zero if doesn't have one.
let hasValue (nullable : Nullable<int64>) =
    nullable.HasValue |> function
        | false -> 0
        | true  -> nullable.Value |> int

/// Optionally returns the value of a Nullable<int>.
let hasValue' (nullable : Nullable<int64>) =
    nullable.HasValue |> function
        | false -> None
        | true  -> nullable.Value |> int |> Some

let testNull = function null -> 0 | (x : string) -> int x

// Googlebot regex.
let googleBotRegex = compileRegex "(?i)googlebot"

let userAgentRegex = compileRegex ".+?:"

// None directive regex.
let noneRegex = compileRegex "(?i)none"

// No index directive regex.
let noIndexRegex = compileRegex "(?i)noindex"

// No follow directive regex.
let noFollowRegex = compileRegex "(?i)nofollow"

// HTML content type regex.
let htmlRegex = compileRegex "(?i)html"

// Anchor tag regex.
let anchorPattern = "(?i)<a.+?>"
let anchorRegex = compileRegex anchorPattern

// Href attribute regex.
let hrefPattern = "(?i)href\\s*=\\s*(\"|\')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"\'\#]+)(\"|\'|\#)"
let hrefRegex = compileRegex hrefPattern

// Absolute Uri regex.
let absoluteUriPattern = "(?i)^https?://[^\"]*"
let absoluteUriRegex = compileRegex absoluteUriPattern

// Meta robots regex.
let metaPattern = "(?i)<meta.+?>"
let metaPatternRegex = compileRegex metaPattern
let robotsPattern = "(?i)robots"
let robotsPatternRegex = compileRegex robotsPattern

/// Builds an internal link regex for the given host.
let internalLinkRegex host =
    let internalUriPattern = "(?i)^https?://" + Regex.Escape(host)
    let regex = Regex(internalUriPattern, RegexOptions.Compiled)
    regex

/// Splits a string list at elements that satisfy a regex pattern.
let splitAtPattern pattern lst =
    let strLst = List.empty
    let regex = compileRegex pattern
    let rec split lst acc b strLst =
        match lst with
            | h :: t ->
                let isMatch = regex.IsMatch(h)
                match isMatch with
                    | true ->
                        match b with
                            | true ->
                                let acc' = h :: acc
                                split t acc' false strLst
                            | false ->
                                let acc' = List.rev acc
                                let strLst' = acc' :: strLst
                                split t [h] true strLst'
                    | false ->
                        let acc' = h :: acc
                        split t acc' false strLst
            | [] ->
                let acc' = List.rev acc
                acc' :: strLst
    split lst [] true strLst

/// Attempts to match a string with a regex.
let matchOption (regex : Regex) str =
    let regexMatch = regex.Match str
    match regexMatch.Success with
        | true  -> Some regexMatch
        | false -> None

/// Returns the trimmed value of a second group in a match object.
let secondGroupValue (regexMatch : Match) = regexMatch.Groups.[1].Value.Trim()

/// Returns the count of a match collection converted to float.
let matchCount (matchCollection : Match seq) = matchCollection |> Seq.length |> float

/// Matches a sting with a regex pattern.
let regexMatches (regex : Regex) str = regex.Matches str |> Seq.cast<Match>

// Img tag regex.
let imgTagRegex = compileRegex "(?is)<img.+?>"

// Alt attribute regex.
let altAttributeRegex = compileRegex "(?i)alt=(\"|')(.+?)(\"|')"

let altAttribute (imgTagMatch : Match) =
    let value = imgTagMatch.Value
    let index = imgTagMatch.Index
    let altMatch = altAttributeRegex.Match value
    altMatch.Success |> function
        | true ->
            let alt = altMatch.Groups.[2].Value.Trim() |> Some
            value, index, alt
        | false -> value, index, None

let constructViolation v i =
    {
        V = v
        Index = i
    }

let constructViolation' v i description (arg : obj) =
    let violation = constructViolation v i
    let description = String.Format(description, arg)
    {
        violation with
            V = { violation.V with Description = description }
    }

let constructViolation'' v i description (args : obj []) =
    let violation = constructViolation v i
    let description = String.Format(description, args)
    {
        violation with
            V = { violation.V with Description = description }
    }