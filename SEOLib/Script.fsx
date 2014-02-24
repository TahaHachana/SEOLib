#r @"..\SEOLib\bin\Release\SEOLib.dll"

open SEOLib

//============
// Html module
//============

let html =
    use client = new System.Net.WebClient()
    client.Encoding <- System.Text.Encoding.UTF8
    client.DownloadString "https://github.com/"

// title
let title = Html.title html

// meta
let meta = Html.meta html

// meta description
let metaDescription = Html.metaDescription html

// meta keywords
let metakeywords = Html.metaKeywords html

// headings
let headings = Html.headings html

// text/HTML ratio
let textHtmlRatio = Html.textHtmlRatio html

//================
// Keywords module
//================

open SEOLib.Keywords

let keywords = Keywords.analyze html

let printKeywordsData (keywords : Keyword list) count =
    keywords
    |> List.filter (fun x -> x.WordsCount = count)
    |> List.iter (fun x -> printfn "%s, %d, %f" x.Combination x.Occurrence x.Density)

let printKeywordsData' = printKeywordsData keywords

let oneKeyword = printKeywordsData' 1
let twoKeywords = printKeywordsData' 2
let threeKeywords = printKeywordsData' 3

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