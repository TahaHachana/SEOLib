namespace SEOLib

open System.Diagnostics
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