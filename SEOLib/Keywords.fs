namespace SEOLib

open Utilities

module Keywords =

    /// <summary>Analyzes the keyword structure of a HTML document.</summary>
    /// <param name="html">The HTML document.</param>
    /// <returns>The keywords data.</returns>
    let analyzeKeywords html =
        let html' = cleanHtml html |> removeStopWords stopWords
        let matchCollection = regexMatches oneKeywordRegex html'
        let count = matchCount matchCollection
        [
            keywordData matchCollection html' count 1.
            twoKeywordsData html' count
            threeKeywordsData html' count
        ] |> Array.concat