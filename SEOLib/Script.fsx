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

// text/HTML ratio
let ratio = Html.textHtmlRatio html

//================
// Keywords module
//================

let keywords = Keywords.analyzeKeywords html

let printKeywordData (keywords : Keyword []) count =
    keywords
    |> Array.filter (fun x -> x.WordsCount = count)
    |> Array.iter (fun x -> printfn "%s, %d, %f" x.Combination x.Occurrence x.Density)

let printKeywordData' = printKeywordData keywords

let oneKeyword = printKeywordData' 1
let twoKeywords = printKeywordData' 2
let threeKeywords = printKeywordData' 3

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

let violations =
    Violations.auditHtml html uri
    |> Async.RunSynchronously
    |> Array.concat

//=================
// Validator module
//=================

let status =
    Validator.isValid "http://www.websharper.com/home"
    |> Async.RunSynchronously

let validationResult = Validator.validateUri "http://www.websharper.com/home"

//=================
// PageSpeed module
//=================
#r @"..\SEOLib\bin\Release\Google.Apis.Pagespeedonline.v1.dll"

let service = PageSpeed.createPagespeedService ""
let pagespeedResult = PageSpeed.runPagespeed service "http://fsharp.org" |> Async.RunSynchronously
let etag = PageSpeed.resultEtag pagespeedResult
let requestUri = PageSpeed.getRequestUri pagespeedResult
let stats = PageSpeed.pagespeedStats pagespeedResult
let statusCode = PageSpeed.getResponseCode pagespeedResult
let score = PageSpeed.getScore pagespeedResult
let rules = PageSpeed.pagespeedRules pagespeedResult