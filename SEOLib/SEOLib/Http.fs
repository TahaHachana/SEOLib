namespace SEOLib
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