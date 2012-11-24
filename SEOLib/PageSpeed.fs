namespace SEOLib

open System
open Google.Apis.Pagespeedonline.v1
open Google.Apis.Pagespeedonline.v1.Data
open Utilities

module PageSpeed =
    
    /// <summary>Initializes a page speed service and sets its API key property.</summary>
    /// <param name="apiKey">The developer API key.</param>
    /// <returns>The page speed service instance.</returns>
    let createPagespeedService apiKey =
        let service = PagespeedonlineService ()
        service.Key <- apiKey
        service

    /// <summary>Runs a page speed test over the specified URI.</summary>
    /// <param name="service">The page speed service to use.</param>
    /// <param name="uriString">The URI that must be tested.</param>
    /// <returns>An asynchronous computation that would run a page speed test over the specified URI.</returns>    
    let runPagespeed (service : PagespeedonlineService) uriString =
        let pagespeedRequest = service.Pagespeedapi.Runpagespeed uriString
        pagespeedRequest.Strategy <- Nullable PagespeedapiResource.Strategy.Desktop
        Async.FromBeginEnd(pagespeedRequest.BeginFetch, pagespeedRequest.EndFetch)

    /// <summary>Returns the ETag of a page speed result.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The ETag of the result.</returns>    
    let resultEtag (result : Result) = result.ETag

    /// <summary>Returns the URI that actually responded to the page speed request.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The final URI that responded to the request.</returns>    
    let getRequestUri (result : Result) = result.Id

    /// <summary>Returns statistics about a page speed result.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The result statistics.</returns> 
    let pagespeedStats (result : Result) =
        let pageStats = getPageStats result
        let cssResponseBytes = getCssBytes pageStats
        let flashResponseBytes = getFlashBytes pageStats
        let htmlResponseBytes = getHtmlBytes pageStats
        let imageResponseBytes = getImageBytes pageStats
        let javascriptResponseBytes = getJavascriptBytes pageStats
        let numberCssResources = getNumberCssResource pageStats
        let numberHosts = getNumberHosts pageStats
        let numberJsResources = getNumberJsResources pageStats
        let numberResources = getNumberResources pageStats
        let numberStaticResourecs = getNumberStaticResourecs pageStats
        let numberTextBytes = getNumberTextBytes pageStats
        let otherBytes = getOtherBytes pageStats
        let totalBytes = getTotalRequestBytes pageStats
        makePagespeedStats cssResponseBytes flashResponseBytes htmlResponseBytes imageResponseBytes javascriptResponseBytes numberCssResources numberHosts numberJsResources numberResources numberStaticResourecs numberTextBytes otherBytes totalBytes

    /// <summary>Returns the HTTP status code of the tested URI.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The HTTP status code.</returns> 
    let getResponseCode (result : Result) = testNullable result.ResponseCode

    /// <summary>Returns the score of a page speed result.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The score value.</returns> 
    let getScore (result : Result) = testNullable result.Score

    /// <summary>Returns the rules associated with the specified page speed result.</summary>
    /// <param name="result">The page speed result.</param>
    /// <returns>The page speed rules list.</returns> 
    let pagespeedRules (result : Result) =
        result.FormattedResults.RuleResults.Values
        |> Seq.toList
        |> List.map makePageSpeedRule