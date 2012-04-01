namespace SEOLib

open System
open System.Net.Http
open System.Net.Http.Headers

module Http =

    type HttpData =
        {
            Location : Uri option
            Headers  : HttpResponseHeaders
            Content  : string
        }

    // Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
    let awaitHttpResponse (client : HttpClient) (requestUri : Uri) = client.GetAsync requestUri |> Async.AwaitTask

    // Returns an asynchronous computation that will wait for the task of sending an async GET request to a Uri.
    let awaitHttpResponse' (client : HttpClient) (requestUrl : string) = client.GetAsync requestUrl |> Async.AwaitTask
    
    // Returns an asynchronous computation that will wait for the task of reading the content of a HTTP response message as a string.
    let awaitReadAsString (response : HttpResponseMessage) = response.Content.ReadAsStringAsync() |> Async.AwaitTask

    // IE9's user-agent string.
    let ie9UserAgent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"

    /// Initializes a HttpClient instance, sets its timeout property to the specified value and adds a user-agent request header. 
    let httpClient timeout =
        let client = new HttpClient()
        client.Timeout <- TimeSpan(0, 0, timeout)
        client.DefaultRequestHeaders.Add("user-agent", ie9UserAgent)
        client

    /// Returns the HTTP response message and content resulting from sending a Get request to the given Uri.
    let fetchUri requestUri =
        async {
            use client = httpClient 10
            try
                let! response = awaitHttpResponse client requestUri
                response.EnsureSuccessStatusCode() |> ignore
                let headers = response.Headers
                let location = headers.Location |> function null -> None | x -> Some x
                let! html = awaitReadAsString response
                return Some { Location = location; Headers = headers; Content = html }
            with _ -> return None
            }

    /// Returns the HTTP response message and content resulting from sending a Get request to the given Url.
    let fetchUrl requestUrl =
        async {
            use client = httpClient 10
            try
                let! response = awaitHttpResponse' client requestUrl
                response.EnsureSuccessStatusCode() |> ignore
                let headers = response.Headers
                let location = headers.Location |> function null -> None | x -> Some x
                let! html = awaitReadAsString response
                return Some { Location = location; Headers = headers; Content = html }
            with _ -> return None
            }