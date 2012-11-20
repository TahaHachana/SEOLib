#r @"..\SEOLib\bin\Release\SEOLib.dll"

open System
open SEOLib
open SEOLib.Types


//============
// Html module
//============

let html =
    use client = new System.Net.WebClient()
    client.DownloadString "http://www.websharper.com/home"

// title    
let titleOption = Html.title html

// meta tags
let metaTags = Html.metaTags html

// meta description
let metaDescOption = Html.metaDescription metaTags

// meta keywords
let metakeysOption = Html.metaKeywords metaTags

// headings
let hs = Html.headings html

//================
// Keywords module
//================

let keywords = Keywords.analyzeKeywords html

let oneKeyword =
    keywords
    |> Array.filter (fun x -> x.WordsCount = 1)
    |> Array.iter (fun x -> printfn "%s, %d, %f" x.Combination x.Occurrence x.Density)

let twoKeywords =
    keywords
    |> Array.filter (fun x -> x.WordsCount = 2)
    |> Array.iter (fun x -> printfn "%s, %d, %f" x.Combination x.Occurrence x.Density)

let threeKeywords =
    keywords
    |> Array.filter (fun x -> x.WordsCount = 3)
    |> Array.iter (fun x -> printfn "%s, %d, %f" x.Combination x.Occurrence x.Density)

//================
// Links module
//================

let links = Links.collectLinks html (System.Uri "http://www.websharper.com/home")

let internalLinks =
    links
    |> List.filter (fun x -> x.Type = Internal)
    |> List.iter (fun x -> printfn "%s" x.URL)

let externalLinks =
    links
    |> List.filter (fun x -> x.Type = External)
    |> List.iter (fun x -> printfn "%s" x.URL)

//==================
// Violations module
//==================

let uri = Uri "http://www.websharper.com/home"

let altAttributeViolations =
    Violations.auditHtml html uri
    |> Async.RunSynchronously
    |> Array.concat

//=================
// Validator module
//=================

let validationResult = Validator.validateUri "http://www.websharper.com/home"
