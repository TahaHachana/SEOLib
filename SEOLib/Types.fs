namespace SEOLib

open System
open System.Net

module Types =
    
    type HtmlAttribute =
        {
            Key   : string
            Value : string
        }

    type Heading =
        | H1 of string
        | H2 of string
        | H3 of string
        | H4 of string
        | H5 of string
        | H6 of string

    type Header =
        {
            Key: string
            Value: string list
        }

    type HttpData =
        {
            RequestUri  : Uri
            StatusCode  : HttpStatusCode
            Headers     : Header list
            MediaType   : string
            Size        : int option 
            Html        : string option
            ElapsedTime : int64
        }

    type Keyword =
        {
            Combination : string
            Occurrence  : int
            Density     : float
        }

    
//
//open System
//open System.Net.Http.Headers
//open Google.Apis.Pagespeedonline.v1.Data
//
//module Types =
//
//    [<AutoOpenAttribute>]
//    module HttpTypes =
//
//        type HttpData =
//            {
//                Location : Uri option
//                Headers  : HttpResponseHeaders
//                Content  : string
//            }
//
//    [<AutoOpenAttribute>]
//    module CrawlerTypes =
//
//        type Message =
//            | Cancel
//            | Done
//            | Mailbox of MailboxProcessor<Message>
//            | Stop
//            | URL     of string option
//
//        type Agent<'T> = MailboxProcessor<'T>
//
//        type MessageAgent = Agent<Message>
//
//        type RogueMode = ON | OFF
//
//    [<AutoOpenAttribute>]
//    module KeywordsTypes =
//
//        type Language =
//            | English
//            | French
//
//    [<AutoOpenAttribute>]
//    module PageSpeedTypes =
//
//        type PSD  = Result.PageStatsData
//
//        type RRDS = Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema
//
//        type UBD  = RRDS.UrlBlocksData
//
//        type RD   = UBD.UrlsData.ResultData
//
//        type RuleResult =
//            {
//                RuleName   : string
//                RuleImpact : float option
//                RuleScore  : int
//                Blocks     : UBD []
//            }
//
//        type RuleData =
//            {
//                RuleName : string
//                Impact   : float option
//                Heading  : string
//                Data     : string []
//            }
//
//    [<AutoOpenAttribute>]
//    module RobotsTypes =
//
//        type Bot =
//            {
//                Name       : string
//                CrawlDealy : float option
//                Allow      : string list
//                Disallow   : string list
//            }
//
//        type Permission = Allowed | Disallowed
//
//        type Follow = DoFollow | NoFollow
//
//        type RobotsDirectives =
//            {
//                Indexing  : Permission
//                Following : Follow
//            }
//
//        type WebPage =
//            {
//                Url     : string
//                Headers : HttpResponseHeaders
//                Html    : string
//                Robots  : RobotsDirectives
//            }
//
//    [<AutoOpenAttribute>]
//    module ViolationsTypes =
//
//        type ViolationLevel =
//            | Error
//            | Information
//            | Warning
//
//        type ViolationCategory =
//            | Content
//            | Performance
//            | SEO
//            | Standards
//
//        type ViolationCode =
//            | AltEmpty
//            | AltMissing
//            | DescriptionEmpty
//            | DescriptionLong
//            | DescriptionMissing
//            | DescriptionMultiple
//            | DescriptionShort
//            | H1Empty
//            | H1Missing
//            | H1Multiple
//            | InvalidMarkup
//            | LargeInlineCss
//            | LargeInlineScript
//            | NoIndex
//            | QueryStringParameterCount
//            | TitleAndDescriptionEquals
//            | TitleEmpty
//            | TitleLong
//            | TitleMissing
//            | TitleShort
//            | TooManyLinks
//            | UseOfRefreshToRedirect
//
//        type MarkupStatus =
//            | Abort
//            | Invalid of string * string
//            | Valid
//
//        type Violation =
//            {
//                Category       : ViolationCategory
//                Code           : ViolationCode
//                Description    : string
//                Heading        : string
//                Level          : ViolationLevel
//                Recommendation : string
//            }
//
//        type Violation' =
//            {
//                V     : Violation
//                Index : int option
//            }