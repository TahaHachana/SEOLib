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
            Key   : string
            Value : string list
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
            WordsCount  : int
            Combination : string
            Occurrence  : int
            Density     : float
        }

    type Follow = DoFollow | NoFollow
    
    type LinkType = External | Internal

    type Link =
        {
            URL    : string
            Anchor : string
            Type   : LinkType
            Follow : Follow
        }

   type ViolationLevel =
        | Error
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
        | LargeInlineCss
        | LargeInlineScript
        | QueryParameterCount
        | RefreshToRedirect
        | TitleEqDescription
        | TitleEmpty
        | TitleLong
        | TitleMissing
        | TitleShort
        | TooManyLinks

    type Violation =
        {
            Category       : ViolationCategory
            Code           : ViolationCode
            Description    : string
            Heading        : string
            Index          : int option
            Level          : ViolationLevel
            Recommendation : string
        }

    type Validity = Valid | Invalid

    type MarkupError =
        {
            Line        : string option
            Col         : string option
            Message     : string
            MessageId   : string
            Explanation : string option
            Source      : string option
        }

    type MarkupValidation =
        {
            Doctype      : string
            Charset      : string
            Status       : Validity
            ErrorCount   : int
            WarningCount : int
            Errors       : MarkupError list option
            Warnings     : MarkupError list option
        }

    type Suggestion =
        {
            Header : string
            Urls   : string list
        }

    type PagespeedStats =
        {
            CssBytes              : int
            FlashBytes            : int
            HtmlBytes             : int
            ImageBytes            : int
            JavascriptBytes       : int
            NumberCssResources    : int64 option
            NumberHosts           : int64 option
            NumberJsResources     : int64 option
            NumberResources       : int64 option
            NumberStaticResourecs : int64 option
            NumberTextBytes       : int
            OtherBytes            : int
            TotalRequestBytes     : int
        }

    type PagespeedRule =
        {
            Name        : string
            Impact      : float option
            Score       : int64 option
            Suggestions : Suggestion list option
        }