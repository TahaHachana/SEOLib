namespace SEOLib

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.RegularExpressions
open SEOLib.Robots
open SEOLib.Types
open SEOLib.Utilities
open Links
open Http

module Crawler =

    let getUrl url (isAllowedFunc : string -> Permission) =
        async {
            let! httpData = fetchUrl url
            match httpData with
                | None      -> return None
                | Some data ->
                    let url' = data.Location |> function Some x -> x.ToString() | None -> url
                    let headers = data.Headers
                    let html = data.Content
                    let permission = isAllowedFunc url'
                    let robotsDirectives = robotsInstructions headers html
                    let robotsDirectives' = { robotsDirectives with Indexing = permission }
                    return
                        Some {
                            Url     = url'
                            Headers = headers
                            Html    = html
                            Robots  = robotsDirectives
                        }
            }

    let crawlUrl isAllowedFunc f host onlyInternal rogueMode url =
        async {
            let! webPage = getUrl url isAllowedFunc
            match webPage with
                | None      -> return []
                | Some data ->
                    do f data
                    let following = data.Robots.Following
                    let links = collectLinks host data onlyInternal
                    let links' =
                        match rogueMode with
                            | OFF ->
                                match following with
                                    | DoFollow ->
                                        links
                                        |> List.filter (fun (_, follow) -> follow = DoFollow)
                                        |> List.map fst
                                    | NoFollow -> []
                            | ON ->
                                links |> List.map fst
                    return links'
            }

    [<Literal>]
    let Gate = 5

    let processMsg (hashset : HashSet<string>) limit (q : ConcurrentQueue<string>) run (mailbox : MessageAgent) =
        let keepRunning =
            match limit with
            | Some limit' ->
                let count = hashset.Count
                count < limit' && run
            | None        -> run
        match keepRunning with
            | true ->
                let url = q.TryDequeue()
                match url with
                    | true, url' ->
                        let isNewUrl = hashset.Contains url' = false
                        match isNewUrl with
                            | true ->
                                hashset.Add url' |> ignore
                                mailbox.Post <| URL(Some url')
                            | false -> mailbox.Post <| URL None
                    | _ -> mailbox.Post <| URL None
            | false -> mailbox.Post Stop

    let spawnSupervisor hashset limit q f =
        MailboxProcessor.Start(fun x ->
            let processMsg' = processMsg hashset limit q
            let rec loop run cancel =
                async {
                    let! msg = x.Receive()
                    match msg with
                        | Mailbox(mailbox) -> 
                            processMsg' run mailbox
                            return! loop run cancel
                        | Stop   -> return! loop false cancel
                        | Cancel -> return! loop false true
                        | _ ->
                            match cancel with
                                | true  -> ()
                                | false -> f |> Async.Start
                            (x :> IDisposable).Dispose()
                }
            loop true false)

    let spawnUrlCollector (q : ConcurrentQueue<string>) (supervisor : MessageAgent) =
        MailboxProcessor.Start(fun y ->
            let rec loop count =
                async {
                    let! msg = y.TryReceive 10000
                    match msg with
                    | Some message ->
                        match message with
                            | URL url ->
                                match url with
                                    | Some url' -> q.Enqueue url'
                                    | None      -> ()
                                return! loop count
                            | _ ->
                                match count with
                                    | Gate ->
                                        supervisor.Post Done
                                        (y :> IDisposable).Dispose()
                                    | _ -> return! loop (count + 1)
                    | None ->
                        supervisor.Post Stop
                        return! loop count
                }
            loop 1)

    let spawnCrawler (urlCollector : MessageAgent) (supervisor : MessageAgent) (crawlFunc : string -> Async<string list>) =
        MailboxProcessor.Start(fun inbox ->
            let rec loop() =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                        | URL url ->
                            match url with
                                | Some url' ->
                                    printfn "crawling %s" url'
                                    let! links = crawlFunc url'
                                    links |> List.iter (fun link -> urlCollector.Post <| URL (Some link))
                                    supervisor.Post(Mailbox inbox)
                                | None -> supervisor.Post(Mailbox inbox)
                            return! loop()
                        | _ ->
                            urlCollector.Post Done
                            (inbox :> IDisposable).Dispose()
                    }
            loop())

    let spawnCanceler (supervisor : MessageAgent) =
        MessageAgent.Start(fun inbox ->
            async {
                let! msg = inbox.Receive()
                supervisor.Post Cancel
                (inbox :> IDisposable).Dispose()
                })

    let crawl (url : string) limit f fWebPage onlyInternal rogueMode =
        let bot = catchAllBot url
        let isAllowedFunc = isAllowed bot
        let q = ConcurrentQueue<string>()
        let set = HashSet<string>()
        let host = hostFromUrl url
        let supervisor = spawnSupervisor set limit q f
        let urlCollector = spawnUrlCollector q supervisor
        let crawlUrl' = crawlUrl isAllowedFunc fWebPage host onlyInternal rogueMode
        let crawlers = [1 .. Gate] |> List.map (fun x -> spawnCrawler urlCollector supervisor crawlUrl')
        let canceler = spawnCanceler supervisor
        urlCollector.Post <| URL (Some url)
        crawlers |> List.iter (fun agent -> agent.Post <| URL None)
        canceler