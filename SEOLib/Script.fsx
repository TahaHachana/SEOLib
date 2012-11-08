#r @"..\SEOLib\bin\Release\SEOLib.dll"

open SEOLib

//============
// Html module
//============

let html =
    use client = new System.Net.WebClient()
    client.DownloadString "http://www.oracle.com/"

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
