

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
      parseResults.successful must beTrue
      parseResults.get.size must equalTo(1)
      parseResults.get.head.nlContent must equalTo(parseInput)
    }
    
  }
}