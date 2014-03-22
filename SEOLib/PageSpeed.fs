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

type PageSpeedRule =
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
        Urls : string list
    }

    static member New header urls =
        {
            Header = header
            Urls = urls
        }

[<AutoOpen>]
module private Utils =

    let placeholderRegex = Regex("\$\d+", RegexOptions.Compiled)

    let rec updateString input (lst:string list) index =
        match lst with
        | head :: tail ->
            let input' = placeholderRegex.Replace(input, head, 1)
            updateString input' tail (index + 1)
        | [] -> input

    type UrlBlock = Data.Result.FormattedResultsData.RuleResultsDataElement.UrlBlocksData

    type UrlData = UrlBlock.UrlsData

    type ArgData = UrlData.ResultData.ArgsData

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
        | _ -> updateString format (argsList args) 0

    type UrlBlockArg = UrlBlock.HeaderData.ArgsData

    let urlBlockHeader (args:UrlBlockArg seq) format =
        let args' =
            args 
            |> Seq.toList
            |> List.map (fun x -> x.Value)
        updateString format args' 0

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
        let header =
            match args with
            | null -> format
            | _ -> urlBlockHeader args format
        RuleDetails.New header <| urlBlockUrls urlBlock

    let pageSpeedRules (result:Data.Result) =
        result.FormattedResults.RuleResults.Values
        |> Seq.map (fun rule ->
            let name = rule.LocalizedRuleName
            let impact = rule.RuleImpact.Value
            let details =
                rule.UrlBlocks
                |> Seq.map formatUrlBlock
                |> Seq.toList
            PageSpeedRule.New name impact details)
        |> Seq.toList

type PageStats = Data.Result.PageStatsData

type PageSpeedReview =
    {
        Uri : string
        Score : int
        Screenshot : string
        Rules : PageSpeedRule list
        Stats : PageStats
    }

    static member New uri score screenshot rules stats =
        {
            Uri = uri
            Score = score
            Screenshot = screenshot
            Rules = rules
            Stats = stats
        }

type SpeedService(apiKey) =

    let initializer = new BaseClientService.Initializer(ApiKey=apiKey)
    let service = new PagespeedonlineService(initializer)

    member __.Review uriString =
        async {
            try
                let pagespeedRequest = service.Pagespeedapi.Runpagespeed uriString
                pagespeedRequest.Strategy <- Nullable PagespeedapiResource.RunpagespeedRequest.StrategyEnum.Desktop
                pagespeedRequest.Screenshot <- Nullable true
                let! result = pagespeedRequest.ExecuteAsync() |> Async.AwaitTask
                let score = int result.Score
                let screenshot =
                    result.Screenshot.Data
                    |> fun x -> Regex("_").Replace(x, "/")
                    |> fun x -> Regex("-").Replace(x, "+")
                    |> fun x -> "data:image/jpeg;base64," + x
                let rules = pageSpeedRules result
                let review = PageSpeedReview.New result.Id score screenshot rules result.PageStats
                return Some review
            with _ -> return None
        }