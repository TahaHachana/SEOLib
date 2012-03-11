namespace SEOLib

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open StopWords
open Types
open Utilities

module Keywords =

    /// Loads a stop words list for the given language.
    let loadStopWords =
        function
            | English -> Array.toList englishStopWords
            | French  -> Array.toList frenchStopWords

    /// Removes stop words form a string.
    let rec removeStopWords str stopWords =
        match stopWords with
        | h :: t ->
            let regex = compileRegex <| "\\b" + h + "\\b" 
            let str'  = regex.Replace(str, "0")
            removeStopWords str' t
        | [] -> str

    let keywordData (matchCollection : Match seq) str count length =
        matchCollection
        |> Seq.map (fun x -> x.Value)
        |> Seq.distinct
        |> Seq.map (fun x -> x, Regex("\\b" + x + "\\b").Matches(str).Count)
        |> Seq.filter (fun (_, x) -> x > 1)
        |> Seq.map (fun (x, y) -> x, y, computeDensity y count length)
        |> Seq.map roundThird
        |> Seq.sortBy (fun (_, x, _) -> x)
        |> Seq.toArray
        |> Array.rev

    let keywordData' regex str count length =
        let matchCollection = regexMatches regex str
        keywordData matchCollection str count length

    let twoKeywordsData str count length = keywordData' twoKeywordsRegex str count length

    let threeKeywordsData str count length = keywordData' threeKeywordsRegex str count length