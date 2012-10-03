#load "HTML.fs"

open SEOLib.HTML

//============
// HTML module
//============

let html =
    use client = new System.Net.WebClient()
    client.DownloadString "http://www.oracle.com/"

// title    
let titleOption = title html

// meta description
let metaTagsOption = tagArray "meta" html
let descriptionOption = metaDescription metaTagsOption

// meta keywords
let keywordsOption = metaKeywords metaTagsOption

// h1
let headingOption = heading html
