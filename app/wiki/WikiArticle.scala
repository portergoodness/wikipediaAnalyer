package wiki

import play.api.libs.json.JsValue

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

case class WikiArticle(plainText: String, wikiMarkup: String, id: String)