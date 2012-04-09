namespace SEOLib

open System
open System.Net
open System.Net.Http.Headers
open System.Text.RegularExpressions
open HTML
open Http
open Types
open Utilities

module Robots =

    /// Constructs the robots.txt full URL.
    let robotstxtUrl url = 
        let uri  = Uri url
        let host = uri.Host
        let url' = "http://" + host + "/robots.txt"
        Uri url'

    /// Downloads the content of a robots.txt file in the form of a string list.
    let downloadRobotstxt (uri : Uri) =
        try
            use client = new WebClient()
            let agent = "Mozilla/5.0 (Windows; U; MSIE 9.0; Windows NT 9.0; en-US)"
            client.Headers.Add("user-agent", agent)
            let txt = client.DownloadString uri
            let lines = txt.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
            let lines' = lines |> Array.filter (fun x -> robotsCommentRegex.IsMatch x = false) |> Some
            lines'
        with _ -> None

    /// Filters robots.txt directives that satisfy a pattern.
    let filterDirectives lst regex =
        lst
        |> List.choose (fun x -> matchOption regex x)
        |> List.map (fun x -> groupValue x 1)
        |> List.filter (fun x -> x <> "")

    /// Creates Bot instances from a string list.
    let bots lst =
        lst
        |> List.map (fun x ->
            let userAgent = List.head x |> userAgentRegex'.Match |> (fun x -> groupValue x 1)
            let crawlDelay =
                x
                |> List.tryPick (fun x -> matchOption crawlDelayRegex x)
                |> function
                    | Some x -> groupValue x 1 |> (fun x -> try float x |> Some with _ -> None)
                    | None   -> None
            let allowDirectives    = filterDirectives x robotsAllowRegex
            let disallowDirectives = filterDirectives x robotsDAllowRegex
            {
                Name = userAgent
                CrawlDealy = crawlDelay
                Allow = allowDirectives
                Disallow = disallowDirectives
            })

    /// Formats the '*' wildcard character, '.' and '?' in regex syntax.
    let directiveRegexPattern directive =
        Regex("\.").Replace(directive, "\\.")
        |> (fun x -> Regex("\?").Replace(x, "\\?"))
        |> (fun x -> Regex("\*").Replace(x, ".*?"))

    /// Determines if a URL is alowed for crawling.
    let isAllowed bot url =
        match bot with
            | None -> Allowed
            | Some bot' ->
                let disallowed = bot'.Disallow
                let emptyDisallow = disallowed.IsEmpty
                match emptyDisallow with
                    | true  -> Allowed
                    | false ->
                        let disallowed' = disallowed |> List.map (fun x -> Disallowed, x)
                        let allowed     = bot'.Allow  |> List.map (fun x -> Allowed   , x)
                        let directives =
                            List.append allowed disallowed'
                            |> List.map (fun (permission, pattern) ->
                                let regex = compileRegex pattern
                                permission, regex, pattern.Length)
                        directives
                        |> List.filter (fun (_, regex, _) -> regex.IsMatch(url))
                        |> function
                            | [] -> Allowed
                            | lst ->
                                lst
                                |> List.maxBy (fun (_, _, length) -> length)
                                |> (fun (permission, _, _) -> permission)

    /// Attempts to find the catchall bot (*) in a Bot list.
    let catchAllBot url =
            let uri = robotstxtUrl url
            let directives = downloadRobotstxt uri
            match directives with
                | None -> None
                | Some directives' ->
                let bot =
                    directives'
                    |> Array.toList
                    |> splitAtPattern userAgentPattern'
                    |> bots
                    |> List.tryFind (fun x -> x.Name = "*")
                match bot with    
                    | None -> None
                    | Some bot' ->
                        {
                            bot' with
                                Allow    = bot'.Allow    |> List.map directiveRegexPattern
                                Disallow = bot'.Disallow |> List.map directiveRegexPattern
                        } |> Some

    let xRobotsTagHeaders (headers : HttpResponseHeaders) =
        [for x in headers do yield x.Key, x.Value |> Seq.toList]
        |> List.filter (fun (key, values) -> key = "X-Robots-Tag")
        |> List.map snd
        |> List.concat
        |> List.filter (fun x -> userAgentRegex.IsMatch x = false)

    let metaRobotsContent html =
        metaTags' html
        |> List.filter metaRobotsRegex.IsMatch
        |> List.map (fun x -> metaContentRegex.Match(x).Groups.[2].Value)
        |> List.map (fun x -> x.Split([|','|], StringSplitOptions.RemoveEmptyEntries))
        |> List.map List.ofArray
        |> List.concat
        |> List.map (fun x -> x.Trim())

    let matchesPattern lst (directiveRegex : Regex) =
        lst
        |> List.tryFind directiveRegex.IsMatch
        |> function None -> false | Some _ -> true

    let robotsInstructions headers html =
        let robotsHeaders = xRobotsTagHeaders headers
        let metasContent = metaRobotsContent html
        let robotsDirectives = List.append robotsHeaders metasContent
        let indexing  =
            matchesPattern robotsDirectives noIndexRegex
            |> function true -> Disallowed | false -> Allowed
        let following =
            matchesPattern robotsDirectives noFollowRegex
            |> function true -> NoFollow | false -> DoFollow
        { Indexing = indexing; Following = following}