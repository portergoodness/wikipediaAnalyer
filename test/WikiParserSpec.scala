



import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import play.api.test._
import play.api.test.Helpers._
import wiki._

/**
 * Spec to test WikiParser
 */
@RunWith(classOf[JUnitRunner])
class WikiParserSpec extends Specification {

  "WikiParser" should {
    
      // since everything is  plain text, it should parse as a single WikiText
    "parse plain text" in {
      val parseInput = "This is a test. Very Exciting! How are you? I hope that your (truly) understand; however boring it might be.  Good night, and comma..."
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(1)
      parseResults.get.head.nlContent must equalTo(parseInput)
    }
    
    "parse single braces as text" in {
      val parseInput = "This is a test. {text}"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(1)
      parseResults.get.head.nlContent must equalTo(parseInput)
    }
    
    "parse double braces as special" in {
      val parseInput = "This is a test. {{text}}"
      val expectedInput = "This is a test. "
      val expectedSpecialRaw = "{{text}}"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(2)
      parseResults.get.head.nlContent must equalTo(expectedInput)
      parseResults.get(1).rawContent must equalTo(expectedSpecialRaw)
    }
    
    "parse braces inside double braces" in {
      val parseInput = "This is a test. {{{text}}}"
      val expectedInput = "This is a test. "
      val expectedSpecialRaw = "{{{text}}}"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(3)
      parseResults.get.head.nlContent must equalTo(expectedInput)
      val special = parseResults.get(1)
      special.children.size must equalTo(1)
      special.children.head.nlContent must equalTo("{text")
      
    }
    
    "only close special on double braces" in {
      val parseInput = "{{This is a {special} test} you see}} After Special"
      val expectedSpecialRaw = "{{This is a {special} test} you see}}"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(2)
      parseResults.get(1).nlContent must equalTo(" After Special")
      val special = parseResults.get(0)
      special.rawContent must equalTo(expectedSpecialRaw)
      
    }
    
    "parse single brackets as text" in {
      val parseInput = "This is a test. [text]"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(1)
      parseResults.get.head.nlContent must equalTo(parseInput)
    }
    
    "parse double brackets as link" in {
      val parseInput = "This is a test. [[text]]"
      val expectedInput = "This is a test. "
      val expectedSpecialRaw = "[[text]]"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(2)
      parseResults.get.head.nlContent must equalTo(expectedInput)
      parseResults.get(1).rawContent must equalTo(expectedSpecialRaw)
    }
    
    "parse brackets inside double brackets" in {
      val parseInput = "This is a test. [[[text]]]"
      val expectedInput = "This is a test. "
      val expectedSpecialRaw = "[[[text]]]"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(3)
      parseResults.get.head.nlContent must equalTo(expectedInput)
      val special = parseResults.get(1)
      special.children.size must equalTo(1)
      special.children.head.nlContent must equalTo("[text")
      
    }
    
    "only close link on double brackets" in {
      val parseInput = "[[This is a [special] test} you see]] After Link"
      val expectedLinkRaw = "[[This is a [special] test} you see]]"
      val parseResults = WikiParser().parse(parseInput)
      parseResults.isDefined must beTrue
      parseResults.get.size must equalTo(2)
      parseResults.get(1).nlContent must equalTo(" After Link")
      val link = parseResults.get(0)
      link.rawContent must equalTo(expectedLinkRaw)
      
    }
    
  }
}