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
open Types
open Utilities

module Http =

    let fetch uri =
        async {
            try
                let stopwatch = Stopwatch()
                stopwatch.Start()
                use client = httpClient()
                use! response = awaitHttpResponse client uri
                let httpContent = response.Content
                let contentHeaders = httpContent.Headers
                let statusCode = response.StatusCode
                let mediaType = contentHeaders.ContentType.MediaType
                let! htmlOption = readHttpContent statusCode mediaType httpContent
                stopwatch.Stop()
                let requestUri = response.RequestMessage.RequestUri
                let headers = httpHeaders response.Headers contentHeaders
                let elapsedTime = stopwatch.ElapsedMilliseconds
                let httpData = makeHttpData requestUri statusCode headers mediaType htmlOption elapsedTime
                return Some httpData
            with _ -> return None
        }



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