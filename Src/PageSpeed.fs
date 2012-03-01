module PageSpeed

open System
open System.Text.RegularExpressions
open Google.Apis.Pagespeedonline.v1
open Google.Apis.Pagespeedonline.v1.Data
open Helpers
open Types

/// Determines wether a speed optimization rule is respected (score = 100).
let isRespected f (rule : RRDS) =
    let ruleScore = rule.RuleScore
    ruleScore.HasValue |> function
        | true -> ruleScore.Value |> int |> f
        | false -> true

/// Returns the impact of a rule result.
let impact (rrds : RRDS) = 
    let ruleImpact = rrds.RuleImpact
    ruleImpact.HasValue |> function
        | false -> None
        | true  -> Some ruleImpact.Value

/// Constructs a RuleResult instance from a RuleResultsDataSchema.
let ruleResult rrds : RuleResult =
    let ruleImpact = impact rrds
    let ruleName = rrds.LocalizedRuleName
    let ruleScore = rrds.RuleScore.Value |> int
    let blocks = rrds.UrlBlocks |> Seq.toArray
    {
        RuleName   = ruleName
        RuleImpact = ruleImpact
        RuleScore  = ruleScore
        Blocks     = blocks
    }

// Place holder regex.
let regex = compileRegex "\$\d+"

/// Replaces place holders within a string with values provided in a list.
let rec updateString str (lst : string list) idx =
    match lst with
    | h :: t ->
        let str' = regex.Replace(str, h, 1)
        let idx' = idx + 1
        updateString str' t idx'
    | [] -> str

let formatBlockData (ubd : UBD) =
    let header = ubd.Header
    let args = header.Args |> function null -> None | args' -> Some args'
    let format = header.Format
    match args with
        | Some args' ->
            let args'' = args' |> Seq.map (fun x -> x.Value) |> Seq.toList
            updateString format args'' 0
        | None -> format

let blockData (rr : RuleResult) = rr.Blocks

let urlData (ubd : UBD) =
    match ubd.Urls with
        | null -> [||]
        | urls ->
            urls
            |> Seq.toArray
            |> Array.map (fun x -> x.Result)

let formatUrlData (rd : RD) =
    let format = rd.Format      
    let args = rd.Args |> function null -> None | args' -> Some args'
    match args with
        | Some args' ->
            let args'' = args' |> Seq.map (fun x -> x.Value) |> Seq.toList
            updateString format args'' 0
        | None -> format

let constructRuleData (w, x, y, z) =
    {
        RuleName = w
        Impact   = x
        Heading  = y
        Data     = z
        }

let ruleResultData (rr : RuleResult) =
    let name = rr.RuleName
    let impact = rr.RuleImpact
    blockData rr
    |> Array.map (fun x ->
        let heading = formatBlockData x
        let data = urlData x |> Array.map formatUrlData
        name, impact, heading, data)

/// Filters the violated speed optimization rules (score < 100).
let violatedRules results =
    let lessThan100 x = x < 100
    let isRespected' = isRespected lessThan100
    results |> Array.filter isRespected'

/// Number of CSS resources.
let cssResourcesCount (stats : PSD) = stats.NumberCssResources |> hasValue

/// Size of CSS response bytes.
let cssResponseBytes (stats : PSD) = stats.CssResponseBytes |> testNull

/// Result etag.
let etag (result : Result) = result.ETag

/// Canonicalized URL after following redirects.
let finalUrl (result : Result) = result.Id

/// Size of Flash response bytes.
let flashResponseBytes (stats : PSD) = stats.FlashResponseBytes |> testNull

/// Hosts count.
let hostsCount (stats : PSD) = stats.NumberHosts |> hasValue

/// Size of HTML response bytes.
let htmlResponseBytes (stats : PSD) = stats.HtmlResponseBytes |> testNull
    
/// HTTP status code.
let httpStatusCode (result : Result) = result.ResponseCode |> hasValue'
    
/// Size of image response bytes.
let imageResponseBytes (stats : PSD) = stats.ImageResponseBytes |> testNull
    
/// Initializes a page speed service.
let initializeService key = PagespeedonlineService(Key = key)

/// Size of JS response bytes.
let jsResponseBytes (stats : PSD) = stats.JavascriptResponseBytes |> testNull

/// Number of JS resources.
let jsResourcesCount (stats : PSD) = stats.NumberJsResources |> hasValue

/// Size of other response bytes.
let otherResponseBytes (stats : PSD) = stats.OtherResponseBytes |> testNull

/// Summary stats for the page.
let pageStats (result : Result) = result.PageStats

/// Returns the violated speed optimization rules in RuleData format.
let processResult results =
    let f = ruleResult >> ruleResultData
    results
    |> violatedRules
    |> Array.map f
    |> Array.concat
    |> Array.map constructRuleData

/// Returns respected speed optimization rules.
let respectedRules results =
    let equals100 x = x = 100
    let isRespected' = isRespected equals100
    results
    |> Array.filter isRespected'
    |> Array.map (fun x -> x.LocalizedRuleName)

/// Runs a page speed test over a URL.
let runPageSpeed (service : PagespeedonlineService) url =
    async {        
        try
            let request = service.Pagespeedapi.Runpagespeed url
            let result = request.Fetch() |> Some
            return result
        with _ -> return None
        }

/// Returns the rule results values of a page speed result.
let ruleResultsValues (result : Result) = result.FormattedResults.RuleResults.Values |> Seq.toArray
    
/// Page speed score.
let score (result : Result) = result.Score |> hasValue'
    
/// Number of static resources.
let staticResourcesCount (stats : PSD) = stats.NumberStaticResources |> hasValue
    
/// Size of text response bytes.
let textResponseBytes (stats : PSD) = stats.TextResponseBytes |> testNull
    
/// Page title.
let title (result : Result) = result.Title
    
/// Size of all request bytes.
let totalRequestBytes (stats : PSD) = stats.TotalRequestBytes

/// Number of all resources.    
let totalResourcesCount (stats : PSD) = stats.NumberResources |> hasValue