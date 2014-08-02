package wiki.parser

object WikiParser {
  def apply() = new WikiParser()
}

class WikiParser extends WikiParsers with WikiTokens with WikiParts {
  
  def text: Parser[String] =
    elem("text content", _.isInstanceOf[TextContent]) ^^ (_.chars)
    
  def wikiToken(chars: String): Parser[String] =
    elem("text content", tkn => tkn.isInstanceOf[WikiToken] && tkn.chars.equals(chars)) ^^ (_.chars)
    
  def top: Parser[List[WikiPart]] = rep1(part)
  def part: Parser[WikiPart] = wikiText | special | link | comment | header
  def special: Parser[WikiSpecial] = wikiToken("{{") ~> rep1(wikiText | special) <~  wikiToken("}}") ^^ (a => new WikiSpecial(a))
  def link: Parser[WikiLink] = wikiToken("[[") ~> rep1(wikiText) <~  wikiToken("]]") ^^ (a => new WikiLink(a))
  def comment: Parser[WikiComment] = wikiToken("<!--") ~> rep1(wikiText) <~  wikiToken("-->") ^^ (a => new WikiComment(a))
  def header: Parser[WikiHeader] = wikiToken("==") ~> rep1(wikiText) <~  wikiToken("==") ^^ (a => new WikiHeader(a))
  def wikiText: Parser[WikiText] = rep1(text) ^^ (a => new WikiText(a.mkString))
  
  
  def parse(input: String): Option[List[WikiPart]] = {
    val a: Input = tokenize(input)
    val parseResults = top(a)
    if (parseResults.successful) {
      Some(parseResults.get)
    } else {
      None
    }
  }
}