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
 */
class WikiParser extends JavaTokenParsers {
  
  override val skipWhitespace = false
  
  def article: Parser[List[WikiPart]] = rep(articleContent)
  
//  def articleParts: Parser[List[WikiPart]] = rep1(articleContent)
  
  def articleContent: Parser[WikiPart] = articleText | leftBrace | leftBracket | headerStart
  
  def articleText: Parser[WikiText] = rep1(articleTextChar) ^^ (a => WikiText(a.mkString))
  
  def articleTextChar: Parser[String] = """[\sa-zA-Z0-9\(\)\.,!\?:;_~'"#\^\$@%&\-\+\*\|<>\()\\/\u00A1-\uFFFF]""".r
  
  
  //****** Special - {{ Surrounded by double braces }} ******
  def leftBrace: Parser[WikiPart] = "{" ~> insideBraces <~ "}" ^^ {
    case content: WikiText => WikiText("{"+content.rawContent+"}")
    case content: WikiSpecial => content
    case content: WikiLink => WikiText("{"+content.rawContent+"}")
  }
  def insideBraces: Parser[WikiPart] = special | contentInBraces
  def contentInBraces: Parser[WikiPart] = articleText | leftBracket
  def special: Parser[WikiSpecial] = "{" ~> article <~ "}" ^^ WikiSpecial
  
  
  //****** Link - [[ Surrounded by double brackets ]] ******
  def leftBracket: Parser[WikiPart] = "[" ~> insideBrackets <~ "]" ^^ {
    case content: WikiText => WikiText("["+content.rawContent+"]")
    case content: WikiSpecial => content
    case content: WikiLink => content
  }
  def insideBrackets: Parser[WikiPart] = link | contentInBrackets
  def contentInBrackets: Parser[WikiPart] = articleText | leftBrace
  def link: Parser[WikiLink] = "[" ~> article <~ "]" ^^ WikiLink
  
  //****** Header - == Surrounded by double equals == ******
  def headerStart: Parser[WikiPart] = "=" ~> insideHeader ^^ {
    case content: WikiText => WikiText("="+content.nlContent)
    case content: WikiHeader => content
    case content: Any => content
  }
  def insideHeader: Parser[WikiPart] = header | contentAfterSingleEquals
  def contentAfterSingleEquals: Parser[WikiPart] = articleContent
  def header: Parser[WikiHeader] = "=" ~> articleText <~ "==" ^^ WikiHeader
  //def headerFinish: Parser[Any] = repN(2, equals)
  //def equals: Parser[String] = "="
  
  
  //****** Parser ******
  def parse(input: String):Option[List[WikiPart]] = {
    val parse = parseAll(article, input)
    if (parse.successful) {
      // merge any subsequent WikiText pieces.  braces and brackets can potentially split up text
      Some(mergeWikiTexts(parse.get))
//      Some(parse.get)
    } else {
      None
    }
  }
  
  def parseTest(input: String) = parseAll(article, input)
  
  def mergeWikiTexts(partList: List[WikiPart]): List[WikiPart] = {
    if (partList.isEmpty) {
      List()
    }
    else if (partList.size == 1) {
      partList
    } else {
      val mergedList = mergeWikiTexts(partList.tail)
      val head = partList.head
      head match {
        case text: WikiText => mergedList.head match {
          case anotherText: WikiText => WikiText(text.nlContent + anotherText.nlContent) +: mergedList.tail
          case notAnother: WikiPart => text +: mergedList
        }
        case other: WikiPart => mergeChildren(other) +: mergedList
      }
    }
  }
  
  def mergeChildren(part: WikiPart) = {
    part.setChildren(mergeWikiTexts(part.children))
  }
}


sealed trait WikiPart {
  def rawContent: String
  def nlContent: String
  def children: List[WikiPart]
  def setChildren(children: List[WikiPart]): WikiPart
}

case class WikiSpecial(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("{{", "", "}}")
  def nlContent = ""
  def children = content
  def setChildren(children: List[WikiPart]) = this.copy(content = children)
}
case class WikiLink(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("[[", "", "]]")
  def nlContent = content.map(a => a.nlContent).mkString
  def children = content
  def setChildren(children: List[WikiPart]) = this.copy(content = children)
}
case class WikiText(content: String) extends WikiPart {
  def rawContent = content
  def nlContent = content
  def children = List()
  def setChildren(children: List[WikiPart]) = this
}
case class WikiHeader(content: WikiText) extends WikiPart {
  def rawContent = "==" + content.rawContent + "=="
  def nlContent = content.nlContent
  def children = content.children
  def setChildren(children: List[WikiPart]) = this
}