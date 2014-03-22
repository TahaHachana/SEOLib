#r @"..\packages\Google.Apis.Pagespeedonline.v1.1.8.1.380\lib\portable-net4+sl4+wp71+win8\Google.Apis.Pagespeedonline.v1.dll"
#r @"..\packages\Newtonsoft.Json.6.0.1\lib\net40\Newtonsoft.Json.dll"
#r @"..\packages\Microsoft.Net.Http.2.2.18\lib\net40\System.Net.Http.Primitives.dll"
#r @"..\SEOLib\bin\Release\SEOLib.dll"

open SEOLib
open System
open System.Net
open System.Text

let requestUri = Uri "https://github.com/"

let html =
    use client = new WebClient()
    client.Encoding <- Encoding.UTF8
    client.DownloadString requestUri

//============
// Html module
//============

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

// links
let links = Html.hyperlinks html requestUri

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

//==================
// Violations module
//==================

let violations = Violations.review html requestUri

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

let speedService = PageSpeed.SpeedService "API Key"

let speedReview =
    speedService.Review "http://fssnip.net"
    |> Async.RunSynchronously

