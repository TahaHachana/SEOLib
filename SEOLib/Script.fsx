#r @"..\packages\Google.Apis.Pagespeedonline.v1.1.9.0.540\lib\portable-net40+sl50+win+wpa81+wp80\Google.Apis.Pagespeedonline.v1.dll"
#r @"..\packages\Newtonsoft.Json.6.0.4\lib\net40\Newtonsoft.Json.dll"
#r @"..\packages\Microsoft.Net.Http.2.2.22\lib\net40\System.Net.Http.Primitives.dll"
#r @"..\SEOLib\bin\Release\SEOLib.dll"

open SEOLib
open System

let requestUri = Uri "http://fsharp.org/testimonials/"

let httpInfo =
    Http.getAsync requestUri
    |> Async.RunSynchronously

let html = httpInfo.Value.Html.Value

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
// PageSpeed module
//=================

let speedService = PageSpeed.SpeedService ""

//speedService.Strategy <- PageSpeed.SpeedStrategy.Mobile

let speedReview =
    speedService.Review "http://fsharp.org"
    |> Async.RunSynchronously