#r "C:\Users\user\Desktop\google-api-dotnet-client-1.1.4344-beta.binary\Services\PagespeedonlineService\Google.Apis.Pagespeedonline.v1.dll"
#r "C:\Users\user\Desktop\google-api-dotnet-client-1.1.4344-beta.binary\Lib\Google.Apis.dll"

#load "Types.fs"
#load "Utilities.fs"
#load "StopWords.fs"
#load "Keywords.fs"
#load "PageSpeed.fs"
#load "HTML.fs"
#load "Links.fs"
#load "Robots.fs"
#load "Crawler.fs"
#load "Violations.fs"

open System
open System.Net
open SEOLib.Types
open SEOLib.Utilities
open SEOLib.Keywords
open SEOLib.PageSpeed
open SEOLib.Links
open SEOLib.Robots
open SEOLib.Crawler
open SEOLib.Violations
open SEOLib.HTML

//============================
// Keywords
//============================

let client = new WebClient()
let html = client.DownloadString "http://en.wikipedia.org/wiki/Bicycle_gearing"
let html' = cleanHtml html
let stopWords = loadStopWords English

let html'' = removeStopWords html' stopWords

let matchCollection = regexMatches oneKeywordRegex html''
let count = matchCount matchCollection

let oneKeyword = keywordData matchCollection html'' count 1.
let twoKeywords = twoKeywordsData html'' count 2.
let threeKeywords = threeKeywordsData html'' count 3.

//============================
// Page Speed
//============================

// Insert your Google API key here.
let key = ""

let service = initializeService key

let url = "http://www.yahoo.com/"
let result = runPageSpeed service url |> Async.RunSynchronously
let result' = result.Value

let tag = etag result'

let results = ruleResultsValues result'
let results' = processResult results
let results'' = respectedRules results

let url' = result'.Id
let stats = pageStats result'
let cssBytes = cssResponseBytes stats
let flashBytes = flashResponseBytes stats
let htmlBytes = htmlResponseBytes stats
let imageBytes = imageResponseBytes stats
let javascriptBytes = jsResponseBytes stats
let cssResources = cssResourcesCount stats
let hosts = hostsCount stats
let jsResources = jsResourcesCount stats
let totalResources = totalResourcesCount stats
let totalStaticResources = staticResourcesCount stats
let otherBytes = otherResponseBytes stats
let textBytes = textResponseBytes stats
let totalBytes = totalRequestBytes stats
let statusCode = httpStatusCode result'
let pageScore = score result'
let pageTitle = title result'

//============================
// Crawler
//============================
let url'' = "http://www.fssnip.net/"
let host = Uri(url'').Host
let collectLinks' = collectLinks true
let g = async { printfn "Done." }
crawl url'' 20 g collectLinks'