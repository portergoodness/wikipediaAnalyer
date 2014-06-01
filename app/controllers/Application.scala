package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import wiki.WikiRetriever

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def testEndpoint(title: String) = Action.async {
	  WikiRetriever().fetchMarkupFromWikipedia(title)
		.map { response =>
		Ok("Got a response: "+ response)
	  }
  }

}