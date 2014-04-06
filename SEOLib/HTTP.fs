module SEOLib.Http

#if INTERACTIVE
#r """..\packages\Microsoft.Net.Http.2.2.18\lib\net40\System.Net.Http.dll"""
#endif

open System
open System.Diagnostics
open System.Net
open System.Net.Http
open System.Net.Http.Headers

type HttpInfo =
    {
        Uri : Uri
        StatusCode : HttpStatusCode
        Headers : HttpHeader list
        MediaType : string
        Size : float<KB> 
        Html : string option
        ElapsedTime : int64<ms>
    }

    static member New uri statusCode headers mediaType size html elapsedTime =
        {
            Uri = uri
            StatusCode = statusCode
            Headers = headers
            MediaType = mediaType
            Size = size
            Html = html
            ElapsedTime = elapsedTime
        }

and HttpHeader =
    {
        Key : string
        Values : string list
    }

    static member New key values =
        {
            Key = key
            Values = values
        }

and [<Measure>] KB

and [<Measure>] ms

[<AutoOpen>]
module private Utils =

    let initStopwatch() =
        let stopwatch = Stopwatch()
        stopwatch.Start()
        stopwatch

    let initHttpClient() =
        let httpClient = new HttpClient()
        let googleBot = "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"
        httpClient.DefaultRequestHeaders.Add("User-Agent", googleBot)
        httpClient

    let httpHeaders (httpResponse:HttpResponseMessage) (contentHeaders:HttpContentHeaders) =
        [
            for x in httpResponse.Headers do
                yield HttpHeader.New x.Key <| Seq.toList x.Value            
            for x in contentHeaders do
                yield HttpHeader.New x.Key <| Seq.toList x.Value            
        ]

    let (|HTML|Other|) =
        function
        | "text/html" -> HTML
        | _ -> Other

    let tryReadStringAsync statusCode mediaType (httpContent:HttpContent) =
        async { 
            try
                match statusCode with
                | HttpStatusCode.OK ->
                    match mediaType with
                    | HTML  ->
                        let! html = Async.AwaitTask <| httpContent.ReadAsStringAsync()
                        return Some html
                    | Other -> return None
                | _ -> return None
            with _ -> return None
        }

    let htmlSize (htmlOption:string option) : float<KB> =
        match htmlOption with
        | None -> 0.0
        | Some html ->
            System.Text.Encoding.UTF8.GetByteCount html
            |> fun x -> System.Math.Round(float x / 1024., 2)
        |> LanguagePrimitives.FloatWithMeasure

    let get (httpClient:HttpClient) (uri:Uri) =
        httpClient.GetAsync uri
        |> Async.AwaitTask

    let elapsedMilliseconds (stopwatch:Stopwatch) =
        stopwatch.ElapsedMilliseconds
        |> LanguagePrimitives.Int64WithMeasure

/// <summary>Sends an asynchronous GET request to the specified URI.</summary>
/// <param name="uri">The request's target URI.</param>
let getAsync (uri:Uri) =
    async {
        try
            let stopwatch = initStopwatch()
            use httpClient = initHttpClient()
            use! httpResponse = get httpClient uri
            stopwatch.Stop()
            let statusCode = httpResponse.StatusCode
            let httpContent = httpResponse.Content
            let contentHeaders = httpContent.Headers
            let mediaType = contentHeaders.ContentType.MediaType
            let! html = tryReadStringAsync statusCode mediaType httpContent
            let httpInfo =
                {
                    Uri = httpResponse.RequestMessage.RequestUri
                    StatusCode = statusCode
                    Headers = httpHeaders httpResponse contentHeaders
                    MediaType = mediaType
                    Size = htmlSize html
                    Html = html
                    ElapsedTime = elapsedMilliseconds stopwatch
                }
            return Some httpInfo
        with _ -> return None
    }