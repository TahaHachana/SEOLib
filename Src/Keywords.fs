module Keywords

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open Helpers
open Types

/// Loads a stop words list for the given language.
let loadStopWords =
    function
        | English -> readAllLines "englishStopWords.txt"
        | French  -> readAllLines "frenchStopWords.txt"

/// Removes stop words form a string.
let rec removeStopWords str stopWords =
    match stopWords with
    | h :: t ->
        let str' = Regex("\\b" + h + "\\b").Replace(str, "0")
        removeStopWords str' t
    | [] -> str

let oneKeywordRegex    = compileRegex "[^0-9\W]+"
let twoKeywordsRegex   = compileRegex "[^0-9\W]+ [^0-9\W]+"
let threeKeywordsRegex = compileRegex "[^0-9\W]+ [^0-9\W]+ [^0-9\W]+"

let keywordData (matchCollection : Match seq) str count length =
    matchCollection
    |> Seq.map (fun x -> x.Value)
    |> Seq.distinct
    |> Seq.map (fun x -> x, Regex("\\b" + x + "\\b").Matches(str).Count)
    |> Seq.filter (fun (_, x) -> x > 1)
    |> Seq.map (fun (x, y) -> x, y, float y / count * 100. * length)
    |> Seq.map roundThird
    |> Seq.sortBy (fun (_, x, _) -> x)
    |> Seq.toArray
    |> Array.rev

let keywordData' regex str count length =
    let matchCollection = regexMatches regex str
    keywordData matchCollection str count length

let twoKeywordsData str count length = keywordData' twoKeywordsRegex str count length

let threeKeywordsData str count length = keywordData' threeKeywordsRegex str count length