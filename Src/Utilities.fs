namespace SEOLib

open System
open System.IO
open System.Net
open System.Text.RegularExpressions
open SEOLib.Types

module internal Utilities =

    /// Compiles a regex.
    let compileRegex pattern = Regex(pattern, RegexOptions.Compiled)

    /// Rounds the third element of a tuple.
    let roundThird (x, y, z : float) = x, y, Math.Round(z, 2)

    /// Returns the value of a Nullable<int> and zero if doesn't have one.
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

    // Regex patterns.
    let absoluteUriPattern   = """(?i)^https?://[^"]*"""
    let altAttributePattern  = """(?i)alt=("|')(.+?)("|')"""
    let aTagPattern          = "(?i)<a.+?>"
    let commentJSCssPattern  = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
    let crawlDelayPattern    = "(?i)^crawl-delay:(.+)"
    let googleBotPattern     = "(?i)googlebot"
    let h1TagPattern         = "(?is)<h1>(.+?)</h1>"
    let hrefPattern          = """(?i)href\\s*=\\s*("|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^"'\#]+)("|'|\#)"""
    let htmlContentPattern   = "(?i)html"
    let htmlTagPattern       = "(?i)<[^>]*>"
    let imgTagPattern        = "(?is)<img.+?>"
    let inlineCssPattern     = "(?is)<style.*?</style>"
    let inlineJsPattern      = "(?is)<script.*?</script>"
    let metaContentPattern   = """content=("|')(.+)("|')"""
    let metaDescContPattern  = """(?is)content=("|')(.+?)("|')"""
    let metaDescPattern      = """(?i)name=("|')description("|')"""
    let metaRefreshPattern   = """(?i)http-equiv=("|')refresh("|')"""
    let metaRobotsPattern    = "(?i)robots"
    let metaTagPattern       = "(?i)<meta.+?>"
    let noFollowPattern      = "(?i)nofollow"
    let noIndexPattern       = "(?i)noindex"
    let noneDirecitvePattern = "(?i)none"
    let oneWordPattern       = "[^0-9\W]+"
    let relAttributePattern  = """rel=("|')(.?)("|')"""
    let robotsAllowPattern   = "(?i)^allow:(.+)"
    let robotsCommentPattern = "^#"
    let robotsDAllowPattern  = "(?i)^disallow:(.+)"
    let threeWordsPattern    = "[^0-9\W]+ [^0-9\W]+ [^0-9\W]+"
    let titleTagPattern      = "(?is)<title>(.+?)</title>"
    let twoWordsPattern      = "[^0-9\W]+ [^0-9\W]+"
    let userAgentPattern     = ".+?:"
    let userAgentPattern'    = "(?i)^user-agent"
    let userAgentPattern''   = "(?i)^user-agent:(.+)"

    // Regex objects.
    let absoluteUriRegex   = compileRegex absoluteUriPattern
    let altAttributeRegex  = compileRegex altAttributePattern
    let anchorRegex        = compileRegex aTagPattern
    let commentJSCssRegex  = compileRegex commentJSCssPattern
    let crawlDelayRegex    = compileRegex crawlDelayPattern
    let googleBotRegex     = compileRegex googleBotPattern
    let h1TagRegex         = compileRegex h1TagPattern
    let hrefRegex          = compileRegex hrefPattern
    let htmlRegex          = compileRegex htmlContentPattern
    let htmlTagRegex       = compileRegex htmlTagPattern
    let imgTagRegex        = compileRegex imgTagPattern
    let inlineCssRegex     = compileRegex inlineCssPattern
    let inlineJsRegex      = compileRegex inlineJsPattern
    let metaContentRegex   = compileRegex metaContentPattern
    let metaDescContRegex  = compileRegex metaDescContPattern
    let metaDescRegex      = compileRegex metaDescPattern
    let metaRefreshRegex   = compileRegex metaRefreshPattern
    let metaRobotsRegex    = compileRegex metaRobotsPattern
    let metaTagRegex       = compileRegex metaTagPattern
    let noFollowRegex      = compileRegex noFollowPattern
    let noIndexRegex       = compileRegex noIndexPattern
    let noneRegex          = compileRegex noneDirecitvePattern
    let oneKeywordRegex    = compileRegex oneWordPattern
    let relAttributeRegex  = compileRegex relAttributePattern
    let robotsAllowRegex   = compileRegex robotsAllowPattern
    let robotsCommentRegex = compileRegex robotsCommentPattern
    let robotsDAllowRegex  = compileRegex robotsDAllowPattern
    let threeKeywordsRegex = compileRegex threeWordsPattern
    let titleTagRegex      = compileRegex titleTagPattern
    let twoKeywordsRegex   = compileRegex twoWordsPattern
    let userAgentRegex     = compileRegex userAgentPattern
    let userAgentRegex'    = compileRegex userAgentPattern''

    /// Compiles an internal link regex for the given URI host.
    let internalLinkRegex host =
        let host' = Regex.Escape host
        let pattern = "(?i)^https?://" + host'
        compileRegex pattern

    /// Splits a string list at elements that satisfy a regex pattern.
    let splitAtPattern pattern lst =
        let strLst = List.empty
        let regex  = compileRegex pattern
        let rec split lst acc b strLst =
            match lst with
                | h :: t ->
                    let isMatch = regex.IsMatch h
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

    /// Returns the trimmed value of a group in a match object.
    let groupValue (regexMatch : Match) (idx : int) = regexMatch.Groups.[idx].Value.Trim()

    /// Returns the count of a match collection converted to float.
    let matchCount (matchCollection : Match seq) = matchCollection |> Seq.length |> float

    /// Matches a sting with a regex pattern.
    let regexMatches (regex : Regex) str = regex.Matches str |> Seq.cast<Match>

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

    let w3cValidatorUri = "http://validator.w3.org/check?uri="

    /// Decodes HTML encoded characters in a string.
    let decodeHtml html = WebUtility.HtmlDecode html

    /// Calculates the density of a keyword.
    let computeDensity count count' length = float count / count' * 100. * length

    /// Returns the host of a URL.
    let hostFromUrl url =
        let uri = Uri url
        uri.Host