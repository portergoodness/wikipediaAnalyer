package wiki.parser

trait WikiParts {
  
  sealed abstract class WikiPart {
    def rawContent: String
    def nlContent: String
    def children: List[WikiPart]
  }

  case class WikiSpecial(content: List[WikiPart]) extends WikiPart {
    def rawContent = content.map(a => a.rawContent).mkString("{{", "", "}}")
    def nlContent = ""
    def children = content
  }
  case class WikiLink(content: List[WikiPart]) extends WikiPart {
    def rawContent = content.map(a => a.rawContent).mkString("[[", "", "]]")
    def nlContent = content.map(a => a.nlContent).mkString
    def children = content
  }
  case class WikiText(content: String) extends WikiPart {
    def rawContent = content
    def nlContent = content
    def children = List()
  }
  case class WikiHeader(content: List[WikiPart]) extends WikiPart {
    def rawContent = nlContent
    def nlContent = content.mkString(""," ","\n")
    def children = content
  }
  case class WikiComment(content: List[WikiPart]) extends WikiPart {
    def rawContent = "<!--" + content.mkString(" ") + "-->"
    def nlContent = ""
    def children = content
  }

}