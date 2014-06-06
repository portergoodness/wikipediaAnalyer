package wiki

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Parses wiki markup from MediaWiki's api into our WikiArticle
 */
object WikiParser {
	def apply() = new WikiParser()
}

/**
 * This class is the parser
 * It consists or rules for parsing.
 * 
 * Our Grammer is:
 * 
 * Article ::= ArticlePart { ArticlePart}
 * ArticlePart ::= Special | Heading | Link | ArticleText | Comment
 * Special ::= "{{" SpecialContent+ "}}"
 * SpecialContent ::= SpecialText | Special
 * SpecialText ::= [^}}]
 * ArticleText ::= //no starting tokens
 */
class WikiParser extends JavaTokenParsers {
  
  override val skipWhitespace = false
  
  def article: Parser[List[WikiPart]] = rep1(articlePart) 
  def articlePart: Parser[WikiPart] = special | normalText  | link
  
  //****** Special *********
  def special: Parser[WikiSpecial] = "{{" ~> rep1(specialContent) <~ "}}" ^^ {
    case content => WikiSpecial(content)
  }
  def specialContent: Parser[WikiPart] = normalText | special
  
  //****** Text ******
  def normalText: Parser[WikiText] = rep1(textParts) ^^ (a => WikiText(a.mkString))
  def textParts: Parser[String] = word | whitespace
  def word: Parser[String] = """[a-zA-Z0-9\(\)\.,!\?:;_~'"#\^\$@%&\-\+\*\|<>\()=\\/\u00A1-\uFFFF]+""".r
  def whitespace: Parser[String] = """\s+""".r
  
  //****** Link ******
  def link: Parser[WikiLink] = "[[" ~> article <~ "]]" ^^ WikiLink
  
  //****** Parser ******
  def parse(input: String) = parseAll(article, input)
}


sealed trait WikiPart {
  def rawContent: String
  def nlContent: String
  def children: List[WikiPart]
}

case class WikiSpecial(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("{{", "", "}}")
  def nlContent = ""
  def children = content
}
case class WikiLink(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("[[", "", "]]")
  def nlContent = content.map(a => a.nlContent).mkString
  def children = content
}
case class WikiText(content: String) extends WikiPart {
  def rawContent = content
  def nlContent = content
  def children = List()
}