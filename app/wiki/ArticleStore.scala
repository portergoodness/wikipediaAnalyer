package wiki

/**
 * A store for articles
 */
object ArticleStore {
  
  var articleCache = Map[String, WikiArticle]()
  
  def getArticle(id: String) = articleCache.get(id)
  
  def storeArticle(article: WikiArticle): Unit = {
    articleCache = articleCache + (article.id -> article)
  }

}