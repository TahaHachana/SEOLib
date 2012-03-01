module Types

open System
open Google.Apis.Pagespeedonline.v1.Data

[<AutoOpenAttribute>]
module CrawlerTypes =

    type Message =
        | Cancel
        | Done
        | Mailbox of MailboxProcessor<Message>
        | Stop
        | URL     of string option

    type Agent = MailboxProcessor<Message>

[<AutoOpenAttribute>]
module KeywordsTypes =

    type Language =
        | English
        | French

[<AutoOpenAttribute>]
module LinksTypes =

    type WebPage =
        {
            ResponseUri : Uri option
            HTML : string
            Headers : (string * string) list
            NoFollow : bool
        }

[<AutoOpenAttribute>]
module PageSpeedTypes =

    type PSD  = Result.PageStatsData

    type RRDS = Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema

    type UBD  = Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema
                      .UrlBlocksData

    type RD   = Result.FormattedResultsData.RuleResultsData.RuleResultsDataSchema
                      .UrlBlocksData
                      .UrlsData.ResultData

    type RuleResult =
        {
            RuleName   : string
            RuleImpact : float option
            RuleScore  : int
            Blocks     : UBD []
        }

    type RuleData =
        {
            RuleName : string
            Impact   : float option
            Heading  : string
            Data     : string []
        }

[<AutoOpenAttribute>]
module RobotsTypes =

    type Bot =
        {
            Name : string
            CrawlDealy : float option
            Allow : string list
            Disallow : string list
        }

    type Permission = Allowed | Disallowed

[<AutoOpenAttribute>]
module ViolationsTypes =

    type ViolationLevel =
        | Error
        | Information
        | Warning

    type ViolationCategory =
        | Content
        | Performance
        | SEO
        | Standards

    type ViolationCode =
        | AltEmpty
        | AltMissing
        | DescriptionEmpty
        | DescriptionLong
        | DescriptionMissing
        | DescriptionMultiple
        | DescriptionShort
        | H1Empty
        | H1Missing
        | H1Multiple
        | InvalidMarkup
        | LargeInlineCss
        | LargeInlineScript
        | NoIndex
        | QueryStringParameterCount
        | TitleAndDescriptionEquals
        | TitleEmpty
        | TitleLong
        | TitleMissing
        | TitleShort
        | TooManyLinks
        | UseOfRefreshToRedirect

    type MarkupStatus =
        | Abort
        | Invalid of string * string
        | Valid

    type Violation =
        {
            Category       : ViolationCategory
            Code           : ViolationCode
            Description    : string
            Heading        : string
            Level          : ViolationLevel
            Recommendation : string
        }

    type Violation' =
        {
            V     : Violation
            Index : int option
        }