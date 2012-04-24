namespace SEOLib

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open Html
open StopWords
open Types
open Utilities

module Keywords =

    /// Loads a stop words list for the given language.
    let private loadStopWords =
        function
            | English -> Array.toList englishStopWords
            | French  -> Array.toList frenchStopWords

    /// Removes stop words form a string.
    let rec private removeStopWords stopWords str =
        match stopWords with
        | h :: t ->
            let regex = compileRegex <| "\\b" + h + "\\b" 
            let str'  = regex.Replace(str, "0")
            removeStopWords t str'
        | [] -> str

    let private keywordData (matchCollection : Match seq) str count length =
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

    let private keywordData' regex str count length =
        let matchCollection = regexMatches regex str
        keywordData matchCollection str count length

    let private twoKeywordsData str count = keywordData' twoKeywordsRegex str count 2.

    let private threeKeywordsData str count = keywordData' threeKeywordsRegex str count 3.

    /// Analyzes the keyword structure of a Web page and returns
    /// a list of one, two and three keywords combinations data.
    let analyzeKeywords html language =
        let stopWords = loadStopWords language
        let html' = cleanHtml html |> removeStopWords stopWords
        let matchCollection = regexMatches oneKeywordRegex html'
        let count = matchCount matchCollection
        let oneKeyword = keywordData matchCollection html' count 1.
        let twoKeywords = twoKeywordsData html' count
        let threeKeywords = threeKeywordsData html' count
        [oneKeyword; twoKeywords; threeKeywords]