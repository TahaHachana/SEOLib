namespace SEOLib

#if INTERACTIVE
#r "System.Net.Http"
#endif

open System
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Diagnostics

module HTTP =

    // IE9 user-agent string.
    let ie9UserAgent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"
    
    /// Initializes a HttpClient instance and adds a user-agent request header. 
    let inline httpClient () =
        let client = new HttpClient()
        client.DefaultRequestHeaders.Add("User-Agent", ie9UserAgent)
        client.MaxResponseContentBufferSize <- int64 1073741824
        client

    /// Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
    let awaitHttpResponse (client : HttpClient) (requestUri : Uri) = client.GetAsync requestUri |> Async.AwaitTask

    /// Returns an asynchronous computation that will wait for the task of reading the content of a HTTP response message as a string.
    let awaitReadAsString (httpContent : HttpContent) = httpContent.ReadAsStringAsync() |> Async.AwaitTask

    type Header = {Key: string; Value: string list}

    /// Initializes a Header instance.
    let inline constructHeader key (value : string seq) = {Key = key; Value = Seq.toList value}
        
    let inline httpHeaders (responseHeaders : HttpResponseHeaders) (contentHeaders : HttpContentHeaders) =
        [
            for x in responseHeaders do
                yield constructHeader x.Key x.Value
            for x in contentHeaders do
                yield constructHeader x.Key x.Value
        ]

    let (|HTML|NotHTML|) mediaType = if mediaType = "text/html" then HTML else NotHTML

    /// Reads HttpContent as a string if its content type is HTML.        
    let readHttpContent statusCode mediaType httpContent =
        async { 
            try
                match statusCode with
                    | System.Net.HttpStatusCode.OK ->
                        match mediaType with
                            | HTML  ->
                                let! html = awaitReadAsString httpContent
                                return Some html
                            | _ -> return None
                    | _ -> return None
            with _ -> return None
        }

    let inline strByteSize (strOption: string option) =
        match strOption with
            | None -> None
            | Some str ->
                Encoding.UTF8.GetByteCount str
                |> float
                |> (fun x -> x / 1024.)
                |> Math.Round
                |> int
                |> Some

    type HttpData =
        {
            RequestUri  : Uri
            StatusCode  : HttpStatusCode
            Headers     : Header list
            MediaType   : string
            Size        : int option 
            Html        : string option
            ElapsedTime : int64
        }

    let makeHttpData requestUri statusCode headers mediaType htmlOption elapsedTime =
        let sizeOption = strByteSize htmlOption
        {
            RequestUri  = requestUri
            StatusCode  = statusCode
            Headers     = headers
            MediaType   = mediaType
            Size        = sizeOption 
            Html        = htmlOption
            ElapsedTime = elapsedTime
        }

    let fetch uri =
        async {
            try
                let stopwatch = Stopwatch()
                stopwatch.Start()
                use client = httpClient()
                use! response = awaitHttpResponse client uri
                let httpContent = response.Content
                let contentHeaders = httpContent.Headers
                let requestUri = response.RequestMessage.RequestUri
                let statusCode = response.StatusCode
                let headers = httpHeaders response.Headers contentHeaders
                let mediaType = contentHeaders.ContentType.MediaType
                let! htmlOption = readHttpContent statusCode mediaType httpContent
                stopwatch.Stop()
                let elapsedTime = stopwatch.ElapsedMilliseconds
                let httpData = makeHttpData requestUri statusCode headers mediaType htmlOption elapsedTime
                return Some httpData
            with _ -> return None
        }

// ==========================================================================================
//    let test = fetch "http://www.oracle.com/" |> Async.RunSynchronously
//    let html = test.Value.Html.Value
//
//    let requestUri = test.Value.RequestUri
//    let size = test.Value.Size.Value
//    let server = test.Value.Headers |> List.tryFind (fun x -> x.Key = "Server")
//    let poweredBy = test.Value.Headers |> List.tryFind (fun x -> x.Key = "X-Powered-By")
//    let elapsedTime = test.Value.ElapsedTime
//
//    let titleOption = title html
//    let metaTagsOption = tagArray "meta" html
//    let descriptionOption = metaDescription metaTagsOption
//    let keywordsOption = metaKeywords metaTagsOption
//    let headingOption = heading html


//
//open System
//open System.Net.Http
//open System.Net.Http.Headers
//open Types
//open Utilities
//
//module Http =
//
//    /// Returns the HTTP response message and content resulting from sending a Get request to the given Uri.
//    let fetchUri requestUri =
//        async {
//            use client = httpClient 10
//            try
//                let! response = awaitHttpResponse client requestUri
//                response.EnsureSuccessStatusCode() |> ignore
//                let headers = response.Headers
//                let location = headers.Location |> function null -> None | x -> Some x
//                let! html = awaitReadAsString response
//                return Some { Location = location; Headers = headers; Content = html }
//            with _ -> return None
//            }
//
//    /// Returns the HTTP response message and content resulting from sending a Get request to the given Url.
//    let fetchUrl requestUrl =
//        async {
//            use client = httpClient 10
//            try
//                let! response = awaitHttpResponse' client requestUrl
//                response.EnsureSuccessStatusCode() |> ignore
//                let headers = response.Headers
//                let location = headers.Location |> function null -> None | x -> Some x
//                let! html = awaitReadAsString response
//                return Some { Location = location; Headers = headers; Content = html }
//            with _ -> return None
//            }
//
//    /// Downloads the content of a Web page.
//    let fetchUri' requestUri =
//        async {
//            use client = httpClient 10
//            try
//                let! response = awaitHttpResponse client requestUri
//                response.EnsureSuccessStatusCode() |> ignore
//                let headers = response.Headers
//                let location = headers.Location |> function null -> None | x -> Some x
//                let! html = awaitReadAsString response
//                return Some html
//            with _ -> return None
//            }