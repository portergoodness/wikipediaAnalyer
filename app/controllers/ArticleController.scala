package controllers

import play.api._
import play.api.mvc._
import wiki.ArticleStore
import wiki.WikiArticle
import wiki.WikiRetriever
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.Json

object ArticleController extends Controller {
  
  /************************************ Search ************************************/
  
  /**
   * Search for wikipedia articles with the provided titles
   * 
   * Returns json mapping each title to an object stating if the article was found, 
   * and if so, a key to use to lookup information about the article
   * 
   * @param title the title to search for
   */
  def search(title: String) = Action.async {
	
    
    WikiRetriever().fetchArticleFromWikipedia(title).map { response => 
    	response match {
    	  case Some(article: WikiArticle) => {
    	    ArticleStore.storeArticle(article)
    	    Ok(Json.toJson(
    	        Map(
    	            "title" -> title,
    	            "id" -> article.id
    	        )))
    	  }
    	  case None => NotFound("No article was found for title: "+title)
    	}
    }
    
  }
  
  /************************************ Article Resource Endpoints ************************************/
  
  /**
   * Retrieve the content for the article
   * 
   * Called from /wiki/:id/content
   * 
   * @param id the article id
   */
  def getContent(id: String) = Action {
    ArticleStore.getArticle(id) match {
      case Some(article: WikiArticle)	=> Ok(article.plainText)
      case None							=> NotFound("ID: "+id+" was not found")
    }
  }
  
  /**
   * Retrieve the disambiguation for the article
   * 
   * Called from /wiki/:id/disambiguation
   * 
   * @param id the article id
   */
  def getDisambiguation(id: String) = Action {
    Ok("Disambiguation: "+id)
  }

}