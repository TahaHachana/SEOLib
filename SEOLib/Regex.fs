module internal SEOLib.Regex

open System.Text.RegularExpressions

let private compile pattern = Regex(pattern, RegexOptions.Compiled)

let altAttr = compile "(?i)alt=(\"|')(.+?)(\"|')"

let ``base`` = compile "(?is)<base.+?>"

let comment = compile "(?s)<!--.*?--\s*>"

let commentScriptCss = compile "(?is)(<!--.*?--\s*>|<script.*?</script>|<style.*?</style>)"

let contentAttr = compile "(?i)content=(\"|')([^\"']+)(\"|')"

let h1 = compile "(?is)<h1[^>]*?>(.+?)</h1>"

let heading = compile "(?is)<h([1-6])[^>]*?>(.+?)</h[1-6]>"

let href = compile "(?i) href\\s*=\\s*(\"|')/?((?!#.*|/\B|mailto:|location\.|javascript:)[^\"'\#]+)(\"|'|\#)"

let httpEquivAttr = compile "(?i)http-equiv=(\"|')([^\"']+)(\"|')"

let hyperlink = compile "(?is)(<a .+?>)(.+?)</a>"

let img = compile "(?is)<img.+?>"

let meta = compile "(?i)<meta .+?>"

let nameAttr = compile "(?i)name=(\"|')([^\"']+)(\"|')"

let nameDescription = compile "(?i)name=(\"|')description(\"|')"

let nameRefresh = compile "(?i)name=(\"|')refresh(\"|')"

let newline = compile "(\n|\r)"

let oneWord = compile @"\b\w+\b"

let relAttr = compile "rel=(\"|')(.+?)(\"|')"

let script = compile "(?is)<script ?[^>]*?>(.*?)</script>"

let scriptCss = compile "(?is)(<script.*?</script>|<style.*?</style>)"

let space = compile " {2,}"

let style = compile "(?is)<style ?[^>]*?>(.*?)</style>"

let tag = compile "(?s)<.+?>"

let threeWords = compile @"\b\w+\b \b\w+\b \b\w+\b"

let title = compile "(?is)<title>(.+?)</title>"

let twoWords = compile @"\b\w+\b \b\w+\b"

let uri = compile "(?i)^https?://[^\"]*"

let uriParam = compile "(\?|&)[^=]+"