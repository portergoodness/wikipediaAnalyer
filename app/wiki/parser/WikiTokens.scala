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
    val tokens: Seq[WikiToken] = tokenizeString(input)
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
   * This method is called iteratively cycles through the input sequence and splits a finite sequence of characters into 
   * a sequence of WikiToken
   */
    def tokenizeString(input: Seq[Char]): Seq[WikiToken] = {
    var state = collection.mutable.Seq[Char]()
    var tokens = collection.mutable.Seq[WikiToken]()
    
    var index = 0
    
    while (index < input.size) {
      // each loop we move down the input, character by character, looking for non-text tokens
      state :+= input(index)
      index = index+1
      val tokenToMatchStr = state.mkString
      
      val matchingTokens = knownTokens.filter(kt => kt._1.startsWith(state))
      
      if (matchingTokens.isEmpty) {
        // existing state does not match any known token.  It must be a text token
        tokens :+= new TextContent(tokenToMatchStr)
        state = collection.mutable.Seq[Char]() //reset state
      } else {
        matchingTokens.find(mt => mt._1.equals(tokenToMatchStr)) match {
          case Some(matchedToken) => {
            // these are exact matches meaning we have found a special token
            tokens :+= matchedToken._2
            state = collection.mutable.Seq[Char]()  //reset state
          }
          case None => {
            // the current state matches part of one or more tokens, but not exactly, keep adding to state
            if (index >= input.size && !state.isEmpty) {
              // we are at the end, create a token with the current state if it is not empty
              tokens :+= new TextContent(tokenToMatchStr)
            }
          }
        }
      }
    }
    
    tokens.toSeq // return immutable sequence
  }
}


  