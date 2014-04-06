module SEOLib.PageSpeed

#if INTERACTIVE
#r @"..\packages\Microsoft.Bcl.Async.1.0.16\lib\net40\Microsoft.Threading.Tasks.dll"
#r @"..\packages\Microsoft.Net.Http.2.2.18\lib\net40\System.Net.Http.Primitives.dll"
#r @"..\packages\Newtonsoft.Json.6.0.1\lib\net40\Newtonsoft.Json.dll"
#r @"..\packages\Google.Apis.1.8.1\lib\net40\Google.Apis.dll"
#r @"..\packages\Google.Apis.Core.1.8.1\lib\portable-net4+sl4+wp71+win8\Google.Apis.Core.dll"
#r @"..\packages\Google.Apis.Pagespeedonline.v1.1.8.1.380\lib\portable-net4+sl4+wp71+win8\Google.Apis.Pagespeedonline.v1.dll"
#endif

open Google.Apis.Services
open Google.Apis.Pagespeedonline.v1
open System
open System.Text.RegularExpressions

type SpeedRule =
    {
        Name : string
        Impact : float
        Details : RuleDetails list
    }

    static member New name impact details =
        {
            Name = name
            Impact = impact
            Details = details
        }

and RuleDetails =
    {
        Header : string
        Hyperlink : string option
        Urls : string list
    }

    static member New header hyperlink urls =
        {
            Header = header
            Hyperlink = hyperlink
            Urls = urls
        }

type SpeedStats =
    {
        CssBytes : int64
        FlashBytes : int64
        HtmlBytes : int64
        ImageBytes : int64
        JavaScriptBytes : int64
        OtherBytes : int64
        TextBytes : int64
        RequestBytes : int64
        CssResources : int
        Hosts : int
        JsResources : int
        StaticResources : int
        TotalResources : int
    }

type SpeedStrategy = Desktop | Mobile

[<AutoOpen>]
module private Utils =

    let printArgs format (args:string list) =
        let rec f format index =
            let argRegex = Regex("\$" + string index)
            match args.Length with
            | x when x = index -> argRegex.Replace(format, args.[index - 1])
            | _ ->
                let format' = argRegex.Replace(format, args.[index - 1])
                f format' (index + 1)
        f format 1

    type private UrlBlock = Data.Result.FormattedResultsData.RuleResultsDataElement.UrlBlocksData

    type private UrlData = UrlBlock.UrlsData

    type private ArgData = UrlData.ResultData.ArgsData

    let argsList (args:ArgData seq) =
        args
        |> Seq.toList
        |> List.map (fun x -> x.Value)

    let formatUrlData (urlData:UrlData) =   
        let result = urlData.Result
        let args = result.Args
        let format = result.Format
        match args with
        | null -> format
        | _ -> printArgs format (argsList args)

    type private UrlBlockArg = UrlBlock.HeaderData.ArgsData

    let urlBlockHeader (args:UrlBlockArg seq) format =
        let args' =
            args 
            |> Seq.toList
            |> List.map (fun x -> x.Value)
        printArgs format args'

    let urlBlockUrls (urlBlock:UrlBlock) =
        match urlBlock.Urls with
        | null -> []
        | urls ->
            urls
            |> Seq.toList
            |> List.map formatUrlData

    let formatUrlBlock (urlBlock:UrlBlock) =
        let header = urlBlock.Header
        let args = header.Args
        let format = header.Format
        let header, hyperlink =
            match args with
            | null -> format, None
            | _ ->
                let hyperlink =
                    args
                    |> Seq.tryFind (fun x -> x.Type = "HYPERLINK")
                    |> function
                    | None -> None
                    | Some x -> Some x.Value
                urlBlockHeader args format, hyperlink
        RuleDetails.New header hyperlink <| urlBlockUrls urlBlock

    let pageSpeedRules (result:Data.Result) =
        result.FormattedResults.RuleResults.Values
        |> Seq.toList
        |> List.map (fun rule ->
            let name = rule.LocalizedRuleName
            let impact = rule.RuleImpact.GetValueOrDefault()            
            let details =
                rule.UrlBlocks
                |> Seq.map formatUrlBlock
                |> Seq.toList
            SpeedRule.New name impact details)

    let pageStats (result:Data.Result) =
        let stats  = result.PageStats
        {
            CssBytes = stats.CssResponseBytes.GetValueOrDefault()
            FlashBytes = stats.FlashResponseBytes.GetValueOrDefault()
            HtmlBytes = stats.HtmlResponseBytes.GetValueOrDefault()
            ImageBytes = stats.ImageResponseBytes.GetValueOrDefault()
            JavaScriptBytes = stats.JavascriptResponseBytes.GetValueOrDefault()
            OtherBytes = stats.OtherResponseBytes.GetValueOrDefault()
            TextBytes = stats.TextResponseBytes.GetValueOrDefault()
            RequestBytes = stats.TotalRequestBytes.GetValueOrDefault()
            CssResources = stats.NumberCssResources.GetValueOrDefault()
            Hosts = stats.NumberHosts.GetValueOrDefault()
            JsResources = stats.NumberJsResources.GetValueOrDefault()
            StaticResources = stats.NumberStaticResources.GetValueOrDefault()
            TotalResources = stats.NumberResources.GetValueOrDefault()
        }

    type private Strategy = PagespeedapiResource.RunpagespeedRequest.StrategyEnum

    let speedStrategy =
        function
        | Desktop -> Nullable Strategy.Desktop
        | Mobile -> Nullable Strategy.Mobile

    let screenshotData (result:Data.Result) =
        result.Screenshot.Data
        |> fun x -> Regex("_").Replace(x, "/")
        |> fun x -> Regex("-").Replace(x, "+")
        |> fun x -> "data:image/jpeg;base64," + x

type SpeedReview =
    {
        Uri : string
        Score : int
        Screenshot : string
        Rules : SpeedRule list
        Stats : SpeedStats
    }

    static member New uri score screenshot rules speedStats =
        {
            Uri = uri
            Score = score
            Screenshot = screenshot
            Rules = rules
            Stats = speedStats
        }

type SpeedService(apiKey) =

    let mutable strategy = Desktop
    let initializer = new BaseClientService.Initializer(ApiKey=apiKey)
    let service = new PagespeedonlineService(initializer)

    /// Gets or sets the speed analysis strategy to use.
    member __.Strategy
        with get() = strategy
        and set newStrategy = strategy <- newStrategy 

    /// Runs page speed analysis on the page at the specified URI.
    member __.Review uriString =
        async {
            try
                let pagespeedRequest = service.Pagespeedapi.Runpagespeed uriString
                pagespeedRequest.Strategy <- speedStrategy strategy
                pagespeedRequest.Screenshot <- Nullable true
                let! result = Async.AwaitTask <| pagespeedRequest.ExecuteAsync()
                let screenshot = screenshotData result
                let rules = pageSpeedRules result
                let stats = pageStats result
                let review = SpeedReview.New result.Id (int result.Score) screenshot rules stats
                return Some review
            with _ -> return None
        }