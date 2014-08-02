package wiki

import play.api.libs.json.JsValue
import wiki.parser.WikiParser

object WikiArticle {
  
  def apply (id: String, wikiMarkup: String) = {
    //todo generate plainText
    new WikiArticle("Plain", wikiMarkup, id)
  }
  
  def apply (json: JsValue) = {
    val markupOption = (json \ "query" \\ "*").headOption
    val content = "Plain"
    val idOption = (json \ "query" \\ "pageid").headOption
    if (markupOption.isDefined && idOption.isDefined) {
      Some(new WikiArticle(content, markupOption.get.toString, idOption.get.toString))
    } else {
      None
    }

  }

}


case class WikiArticle(plainText: String, wikiMarkup: String, id: String) {
  

  protected val parser = new WikiParser()
  
  protected val parts = parser.parse(wikiMarkup)
  
  def articleContent = parser.parse(wikiMarkup)
}