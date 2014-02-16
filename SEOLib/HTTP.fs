module SEOLib.Http

#if INTERACTIVE
#r """..\packages\Microsoft.Net.Http.2.2.18\lib\net40\System.Net.Http.dll"""
#endif

open System
open System.Diagnostics
open System.Net
open System.Net.Http
open System.Net.Http.Headers

let private initStopwatch() =
    let stopwatch = Stopwatch()
    stopwatch.Start()
    stopwatch

let private initHttpClient() =
    let httpClient = new HttpClient()
    httpClient.DefaultRequestHeaders.Add("User-Agent", "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
    httpClient

type HttpInfo =
    {
        RequestUri : Uri
        StatusCode : HttpStatusCode
        Headers : HttpHeader list
        MediaType : string
        Size : float<KB> 
        Html : string option
        ElapsedTime : int64<ms>
    }

    static member New requestUri statusCode headers mediaType size html elapsedTime =
        {
            RequestUri = requestUri
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

let private httpHeaders (httpResponse:HttpResponseMessage) (contentHeaders:HttpContentHeaders) =
    [
        for x in httpResponse.Headers do
            yield HttpHeader.New x.Key <| Seq.toList x.Value            
        for x in contentHeaders do
            yield HttpHeader.New x.Key <| Seq.toList x.Value            
    ]

let private (|HTML|Other|) = function
    | "text/html" -> HTML
    | _ -> Other

let private tryReadStringAsync statusCode mediaType (httpContent:HttpContent) =
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

let private htmlSize (html:string option) : float<KB> =
    match html with
    | None -> 0.0
    | Some str ->
        System.Text.Encoding.UTF8.GetByteCount str
        |> fun x -> System.Math.Round(float x / 1024., 2)
    |> LanguagePrimitives.FloatWithMeasure

let getAsync (requestUri:Uri) =
    async {
        try
            let stopwatch = initStopwatch()
            use httpClient = initHttpClient()
            use! httpResponse =
                httpClient.GetAsync requestUri
                |> Async.AwaitTask
            stopwatch.Stop()
            let requestUri = httpResponse.RequestMessage.RequestUri
            let statusCode = httpResponse.StatusCode
            let httpContent = httpResponse.Content
            let contentHeaders = httpContent.Headers
            let headers = httpHeaders httpResponse contentHeaders
            let mediaType = contentHeaders.ContentType.MediaType
            let! html = tryReadStringAsync statusCode mediaType httpContent
            let size:float<KB> = htmlSize html
            let elapsedTime:int64<ms> =
                stopwatch.ElapsedMilliseconds
                |> LanguagePrimitives.Int64WithMeasure
            let httpInfo = HttpInfo.New requestUri statusCode headers mediaType size html elapsedTime
            return Some httpInfo
        with _ -> return None
    }