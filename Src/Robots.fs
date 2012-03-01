module Robots

open System
open System.Net
open System.Text.RegularExpressions
open Helpers
open Types

// Directives regex objects.
let commentRegex    = compileRegex "^#"
let userAgentRegex  = compileRegex "(?i)^user-agent:(.+)"
let crawlDelayRegex = compileRegex "(?i)^crawl-delay:(.+)"
let allowRegex      = compileRegex "(?i)^allow:(.+)"
let disallowRegex   = compileRegex "(?i)^disallow:(.+)"

/// Constructs the robots.txt URL.
let robotstxtUrl url = 
    let uri = Uri url
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
        let lines' = lines |> Array.filter (fun x -> commentRegex.IsMatch x = false) |> Some
        lines'
    with _ -> None

/// Filters robots.txt directives that satisfy a pattern.
let filterDirectives lst regex =
    lst
    |> List.choose (fun x -> matchOption regex x)
    |> List.map secondGroupValue
    |> List.filter (fun x -> x <> "")

/// Creates Bot instances from a string list.
let bots lst =
    lst
    |> List.map (fun x ->
        let userAgent = List.head x |> userAgentRegex.Match |> secondGroupValue
        let crawlDelay =
            x
            |> List.tryPick (fun x -> matchOption crawlDelayRegex x)
            |> function
                | Some x -> secondGroupValue x |> (fun x -> try float x |> Some with _ -> None)
                | None   -> None
        let allowDirectives    = filterDirectives x allowRegex
        let disallowDirectives = filterDirectives x disallowRegex
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
        | None -> true
        | Some bot' ->
            let disallowed = bot'.Disallow
            let emptyDisallow = disallowed.IsEmpty
            match emptyDisallow with
                | true  -> true
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
                        | [] -> true
                        | lst ->
                            lst
                            |> List.maxBy (fun (_, _, length) -> length)
                            |> (fun (permission, _, _) -> permission)
                            |> function Allowed -> true | Disallowed -> false

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
                |> splitAtPattern "(?i)^user-agent"
                |> bots
                |> List.tryFind (fun x -> x.Name = "*")
            match bot with    
                | None -> None
                | Some bot' ->
                    let bot'' =
                        {
                            bot' with
                                Allow    = bot'.Allow    |> List.map directiveRegexPattern
                                Disallow = bot'.Disallow |> List.map directiveRegexPattern
                        } |> Some
                    bot''