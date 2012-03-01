module Crawler

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Net
open System.Text.RegularExpressions
open Types
open Robots

[<Literal>]
let Gate = 5

let processMsg (hashset : HashSet<string>) limit (q : ConcurrentQueue<string>) run (mailbox : Agent) =
    let count = hashset.Count
    let keepRunning = count < limit - 1 && run
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
                        printfn "Supervisor is done."
                        (x :> IDisposable).Dispose()
            }
        loop true false)

let spawnUrlCollector (q : ConcurrentQueue<string>) (supervisor : Agent) =
    MailboxProcessor.Start(fun y ->
        let rec loop count =
            async {
                let! msg = y.TryReceive(10000)
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
                                    printfn "URL collector is done."
                                | _ -> return! loop (count + 1)
                | None ->
                    supervisor.Post Stop
                    return! loop count
            }
        loop 1)

let spawnCrawler id host (urlCollector : Agent) (supervisor : Agent) isAllowedFunc f =
    MailboxProcessor.Start(fun inbox ->
        let rec loop() =
            async {
                let! msg = inbox.Receive()
                match msg with
                    | URL url ->
                        match url with
                            | Some url' ->
                                let isAllowed = isAllowedFunc url'
                                match isAllowed with
                                    | true ->
                                        let links = f url' host
                                        links |> List.iter (fun link -> urlCollector.Post <| URL (Some link))
                                        printfn "%A crawled by agent %d." url' id
                                    | false -> ()
                                supervisor.Post(Mailbox inbox)
                            | None -> supervisor.Post(Mailbox inbox)
                        return! loop()
                    | _ ->
                        urlCollector.Post Done
                        printfn "Agent %d is done." id
                        (inbox :> IDisposable).Dispose()
                }
        loop())

let hostFromUrl url =
    let uri = Uri url
    uri.Host

let crawl (url : string) limit f g =
    let bot = catchAllBot url
    let h = isAllowed bot
    let q = ConcurrentQueue<string>()
    let set = HashSet<string>()
    let host = hostFromUrl url
    let supervisor = spawnSupervisor set limit q f
    let urlCollector = spawnUrlCollector q supervisor
    let crawlers = [1 .. Gate] |> List.map (fun x -> spawnCrawler x host urlCollector supervisor h g)
    crawlers.Head.Post <| URL(Some url)
    crawlers.Tail |> List.iter (fun agent -> agent.Post <| URL None)