package wiki.parser

import scala.util.parsing.combinator.token.Tokens
import scala.util.parsing.input.Position
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Reader

trait WikiTokens extends Tokens {
  
  /**
   * These are the Tokens we will use for our parser
   */
  sealed abstract class WikiToken extends Token {
  

  }

  case class DoubleLeftBrace() extends WikiToken {
    override def chars = "{{"
  }
  case class DoubleRightBrace() extends WikiToken {
    def chars = "}}"
  }
  case class DoubleLeftBracket() extends WikiToken {
    override def chars = "[["
  }
  case class DoubleRightBracket() extends WikiToken {
    def chars = "]]"
  }
  case class DoubleLeftParen() extends WikiToken {
    override def chars = "(("
  }
  case class DoubleRightParen() extends WikiToken {
    def chars = "))"
  }
  case class OpenComment() extends WikiToken {
    override def chars = "<!--"
  }
  case class CloseComment() extends WikiToken {
    def chars = "-->"
  }
  case class DoubleEquals() extends WikiToken {
    def chars = "=="
  }
  
  case class TextContent(chars: String) extends WikiToken 
  
  /**
   * This scanner is a Reader for our custom tokens.
   * The standard and java token parsers did not read ahead and could not distinguish
   * between <! and <!--, at least not easily.
   * 
   * This is our own custom tokenizer so our grammers can be simpler
   */
  class WikiScanner(tokens: Seq[WikiToken], tokenOffset: Int, charOffset: Int, src: CharSequence) extends Reader[WikiToken] {

    override def source = src
  
    override def offset = charOffset
  
    def first: WikiToken = tokens(tokenOffset)
  
    def rest: WikiScanner = new WikiScanner(tokens, tokenOffset+1, charOffset+first.chars.size-1, src)
  
    def pos: Position = new OffsetPosition(source, offset)

    def atEnd: Boolean = tokenOffset >= tokens.length 

  }  
  
  /**
   * Generator for WikiScanner.  Call this method to turn a string into a WikiScanner of tokens
   */
  def tokenize(input: String):  WikiScanner = {
    val tokens: Seq[WikiToken] = tokenizeString(input, Seq())
    new WikiScanner(tokens, 0, 0, input)
  }
  
  /**
   * These are Added tokens to represent Wiki Constructs
   */
  private val knownTokens = Map(
      "{{" -> new DoubleLeftBrace,
      "}}" -> new DoubleRightBrace,
      "[[" -> new DoubleLeftBracket,
      "]]" -> new DoubleRightBracket,
      "((" -> new DoubleLeftParen,
      "))" -> new DoubleRightParen,
      "<!--" -> new OpenComment,
      "==" -> new DoubleEquals,
      "-->" -> new CloseComment)
  
  /**
   * This method is called recursively and splits a finite sequence of characters into 
   * a sequence of WikiToken
   */
    def tokenizeString(input: Seq[Char], state: Seq[Char]): Seq[WikiToken] = {
    
    if (input.isEmpty) {
      if (state.isEmpty) {
        Seq()
      } else {
        Seq(new TextContent(state.mkString))
      }
    }
    else {
      
      val tokenToMatch = state :+ input.head
      val tokenToMatchStr = tokenToMatch.mkString
      val matchingTokens = knownTokens.filter(kt => kt._1.startsWith(tokenToMatch))
      
      if (matchingTokens.isEmpty) {
        new TextContent(tokenToMatchStr) +: tokenizeString(input.tail, Seq())
      } else {
        matchingTokens.find(mt => mt._1.equals(tokenToMatchStr)) match {
          case Some(matchedToken) => matchedToken._2 +: tokenizeString(input.tail, Seq())
          case None => tokenizeString(input.tail, tokenToMatch)
        }
      }
    }
  }
}


  