package wiki

import scala.concurrent.Future
import play.api.libs.ws.WS
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Provides a default instance of WikiRetriever, ready to retrieve content for WikipediaAnalyer
 */
object WikiRetriever {
  
  /**
   * Creates a WikiRetriever with the defaults
   */
	def apply(): WikiRetriever = new WikiRetriever(
	    "WikipediaAnalyer/alpha (http://github.com/portergoodness/wikipediaAnalyer)",
		"http://en.wikipedia.org/w/api.php",
		List(("format", "json"),
			("action", "query"),
			("prop", "revisions"),
			("rvprop", "content")),
		10000)
  

}

/**
 * Retrieves content from wikipedia
 */
case class WikiRetriever(userAgentDescription: String, wikiUrl: String, queryParams: List[(String, String)], timeout: Int) {
  
	def withUserAgent(userAgent: String) = this.copy(userAgentDescription = userAgent)
	def withWikiUrl(url: String) = this.copy(wikiUrl = url)
	def withQueryParams(params: (String, String)*) = this.copy(queryParams = params.toList)
	def withTimeout(to: Int) = this.copy(timeout = to)
	
	/**
	   * Searches the MediaWiki api for an article with the provided title
	   * 
	   * @param title An article title to search for
	   */
	def fetchMarkupFromWikipedia(title: String): Future[String] = {
	  
	  val queryParamsWithTitle = queryParams :+ ("titles", title)

		WS.url(wikiUrl)
			.withHeaders(("user_agent", userAgentDescription))
			.withQueryString(queryParamsWithTitle:_*)
			.withRequestTimeout(timeout)
			.get()
			.map { response =>
				val responseStr = response.json.toString
				//TODO: drop the rest of the json and only keep the content
				responseStr
			}
	}
}
