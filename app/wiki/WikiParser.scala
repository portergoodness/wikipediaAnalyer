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
  
  //TODO: Refactor charsToNotParse to be a Set instead of a List
  def article(charsToNotParse: List[String] = List()): Parser[List[WikiPart]] = rep(articleContent(charsToNotParse))
  def articleContent(charsToNotParse: List[String]): Parser[WikiPart] = articleText(charsToNotParse) | leftBrace(charsToNotParse) | leftBracket(charsToNotParse) | headerStart(charsToNotParse)
  def articleText(charsToNotParse: List[String]): Parser[WikiText] = rep1(articleTextChar(charsToNotParse)) ^^ (a => WikiText(a.mkString))
  val acceptableCharacterPattern = """\sa-zA-Z0-9\(\)\.,!\?:;_~'"#\^\$@%&\-\+\*\|<>\\"""
    //TODO: Add equals, > and - to conditionally acceptable
  val conditiallyAcceptableCharacters = List("""\]""","""\}""")
  def articleTextChar(charsToNotParse: List[String]): Parser[String] = ("""["""+acceptableCharacterPattern+conditiallyAcceptableCharacters.filter(a => !charsToNotParse.contains(a)).mkString+"""]""").r

  
  //****** Special - {{ Surrounded by double braces }} ******
  def leftBrace(charsToNotParse: List[String]): Parser[WikiPart] = "{" ~> insideBraces(charsToNotParse) ^^ {
    case content: WikiSpecial => content
    case content: Any => content.prependPart(WikiText("{"))
  }
  def insideBraces(charsToNotParse: List[String]): Parser[WikiPart] = special(charsToNotParse) | articleContent(charsToNotParse)
  def special(charsToNotParse: List[String]): Parser[WikiSpecial] = "{" ~> specialContent(charsToNotParse :+ """\}""") ^^ (a => WikiSpecial(a.dropRight(2)))
  def specialContent(charsToNotParse: List[String]): Parser[List[WikiPart]] = article(charsToNotParse) ~ rightBrace ~ endOfSpecial(charsToNotParse) ^^ {
    case firstContent ~ brace ~ endContent => (firstContent :+ brace) ++ endContent
  }
  def endOfSpecial(charsToNotParse: List[String]): Parser[List[WikiPart]] = finalCloseBrace(charsToNotParse) | specialContent(charsToNotParse)
  def finalCloseBrace(charsToNotParse: List[String]): Parser[List[WikiPart]] = rightBrace ^^ (a => List(a))
  def rightBrace: Parser[WikiText] = "}" ^^ WikiText
  
  
  //****** Link - [[ Surrounded by double brackets ]] ******
  def leftBracket(charsToNotParse: List[String]): Parser[WikiPart] = "[" ~> insideBrackets(charsToNotParse) ^^ {
    case content: WikiLink =>  content
    case content: Any => content.prependPart(WikiText("["))
  }
  def insideBrackets(charsToNotParse: List[String]): Parser[WikiPart] = link(charsToNotParse) | articleContent(charsToNotParse)
  def link(charsToNotParse: List[String]): Parser[WikiLink] = "[" ~> linkContent(charsToNotParse :+ """\]""") ^^ (a => WikiLink(a.dropRight(2)))
  def linkContent(charsToNotParse: List[String]): Parser[List[WikiPart]] = article(charsToNotParse) ~ rightBracket ~ endOfLink(charsToNotParse) ^^ {
    case firstContent ~ bracket ~ endContent => (firstContent :+ bracket) ++ endContent
  }
  def endOfLink(charsToNotParse: List[String]): Parser[List[WikiPart]] = finalCloseBracket(charsToNotParse) | linkContent(charsToNotParse)
  def finalCloseBracket(charsToNotParse: List[String]): Parser[List[WikiPart]] = rightBracket ^^ (a => List(a))
  def rightBracket: Parser[WikiText] = "]" ^^ WikiText
  
  
  //****** Header - == Surrounded by double equals == ******
  //TODO: Add tests to header
  def headerStart(charsToNotParse: List[String]): Parser[WikiPart] = "=" ~> insideHeader(charsToNotParse) ^^ {
    case content: WikiHeader => content
    case content: Any => content.prependPart(WikiText("="))
  }
  def insideHeader(charsToNotParse: List[String]): Parser[WikiPart] = header(charsToNotParse) | contentAfterSingleEquals(charsToNotParse)
  def contentAfterSingleEquals(charsToNotParse: List[String]): Parser[WikiPart] = articleContent(charsToNotParse)
  def header(charsToNotParse: List[String]): Parser[WikiHeader] = "=" ~> articleText(charsToNotParse) <~ "==" ^^ WikiHeader
  
  
  //****** Comment - <!-- XML Comment --> ******
//  def leftAngleBracket(charsToNotParse: List[String]): Parser[WikiPart] = "<" ~> insideAngleBracket(charsToNotParse) ^^ {
//    case content: WikiComment => content
//    case content: Any => content.prependPart(WikiText("<"))
//  }
//  def insideAngleBracket(charsToNotParse: List[String]): Parser[WikiPart] = angleBang(charsToNotParse) | articleContent(charsToNotParse)
//  def angleBang(charsToNotParse: List[String]): Parser[WikiPart] = "!" ~> insideAngleBang(charsToNotParse) ^^ {
//    case content: WikiComment => content
//    case content: Any => content.prependPart(WikiText("!"))
//  }
//  def insideAngleBang(charsToNotParse: List[String]): Parser[WikiPart] = angleBangDash(charsToNotParse) | articleContent(charsToNotParse)
//  def angleBangDash(charsToNotParse: List[String]): Parser[WikiPart] = "-" ~> insideAngleBangDash(charsToNotParse) | articleContent(charsToNotParse) ^^ {
//    case content: WikiComment => content
//    case content: Any => content.prependPart(WikiText("-"))
//  }
//  def insideAngleBangDash(charsToNotParse: List[String]): Parser[WikiPart] = comment(charsToNotParse) | articleContent(charsToNotParse)
//  def comment(charsToNotParse: List[String]): Parser[WikiComment] = "".r ^^ WikiComment
  
  
  //****** Parser ******
  def parse(input: String):Option[List[WikiPart]] = {
    val parse = parseAll(article(), input)
    if (parse.successful) {
      // merge any subsequent WikiText pieces.  braces and brackets can potentially split up text
      Some(mergeWikiTexts(parse.get))
    } else {
      None
    }
  }
  
  def parseTest(input: String) = parseAll(article(), input)
  
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
  def prependPart(part: WikiPart): WikiPart
  def appendPart(part: WikiPart): WikiPart
}

case class WikiSpecial(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("{{", "", "}}")
  def nlContent = ""
  def children = content
  def setChildren(children: List[WikiPart]) = this.copy(content = children)
  def prependPart(part: WikiPart) = this.copy(content = part +: content)
  def appendPart(part: WikiPart) = this.copy(content = content :+ part)
}
case class WikiLink(content: List[WikiPart]) extends WikiPart {
  def rawContent = content.map(a => a.rawContent).mkString("[[", "", "]]")
  def nlContent = content.map(a => a.nlContent).mkString
  def children = content
  def setChildren(children: List[WikiPart]) = this.copy(content = children)
  def prependPart(part: WikiPart) = this.copy(content = part +: content)
  def appendPart(part: WikiPart) = this.copy(content = content :+ part)
}
case class WikiText(content: String) extends WikiPart {
  def rawContent = content
  def nlContent = content
  def children = List()
  def setChildren(children: List[WikiPart]) = this
  def prependPart(part: WikiPart) = this.copy(content = part.nlContent + content)
  def appendPart(part: WikiPart) = this.copy(content = content + part.nlContent)
}
case class WikiHeader(content: WikiText) extends WikiPart {
  def rawContent = "==" + content.rawContent + "=="
  def nlContent = content.nlContent
  def children = content.children
  def setChildren(children: List[WikiPart]) = this
  def prependPart(part: WikiPart) = this.copy(content = content.prependPart(part))
  def appendPart(part: WikiPart) = this.copy(content = content.appendPart(part))
}
case class WikiComment(content: WikiText) extends WikiPart {
  def rawContent = "<!--" + content.rawContent + "-->"
  def nlContent = ""
  def children = content.children
  def setChildren(children: List[WikiPart]) = this
  def prependPart(part: WikiPart) = this.copy(content = content.prependPart(part))
  def appendPart(part: WikiPart) = this.copy(content = content.appendPart(part))
}
