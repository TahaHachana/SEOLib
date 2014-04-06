module SEOLib.Keywords

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

type Keyword =
    {
        WordsCount : int
        Combination : string
        Occurrence : int
        Density : float
    }
    
    static member New wordsCount combination occurrence density =
        {
            WordsCount = wordsCount
            Combination = combination
            Occurrence = occurrence
            Density = density
        }

[<AutoOpen>]
module private Utils =

    let stripMarkup html =
        Regex.commentScriptCss.Replace(html, "*")
        |> fun x -> Regex.tag.Replace(x, "*").ToLower()

    let stopwords =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream("EnglishStopWords.txt")
        use reader = new StreamReader(stream)
        [while not reader.EndOfStream do yield reader.ReadLine()]

    let rec stripStopwords stopwords input =
        match stopwords with
        | head :: tail ->
            let input' = Regex("\\b" + head + "\\b").Replace(input, "*")
            stripStopwords tail input'
        | [] -> input

    let computeDensity count keywordsCount wordsCount =
        float count / keywordsCount * 100. * (float wordsCount)
        |> fun x -> Math.Round(x, 2)

    let keywords (matchCollection:seq<Match>) input keywordsCount wordsCount =
        matchCollection
        |> Seq.map (fun x -> x.Value)
        |> Seq.distinct
        |> Seq.toList
        |> List.map (fun x ->
            let occurrence = Regex("\\b" + x + "\\b").Matches(input).Count
            let density = computeDensity occurrence keywordsCount wordsCount
            Keyword.New wordsCount x occurrence density)

    let keywords' (regex:Regex) input keywordsCount wordsCount =
        let matchCollection = regex.Matches input |> Seq.cast<Match>
        keywords matchCollection input keywordsCount wordsCount

/// <summary>Analyzes the keywords of a HTML document.</summary>
/// <param name="html">The HTML document.</param>
let analyze html =
    let html' =
        stripMarkup html
        |> stripStopwords stopwords
    let oneWordMatches =
        Regex.oneWord.Matches html'
        |> Seq.cast<Match>
    let keywordsCount = float <| Seq.length oneWordMatches
    List.concat [
        keywords oneWordMatches html' keywordsCount 1
        keywords' Regex.twoWords html' keywordsCount 2
        keywords' Regex.threeWords html' keywordsCount 3
    ]