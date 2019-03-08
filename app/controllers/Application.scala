package controllers

import java.time.LocalDateTime

import javax.xml.parsers.SAXParser
import play.api.mvc._
import play.api.libs.ws.{WS, WSResponse}
import play.api.Play.current

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.{Elem, Node, NodeSeq}

object Application extends Controller {

  def index = Action.async {

    val opml: Elem = scala.xml.XML.loadFile("conf/subscription_manager.xml")
    val outlines: NodeSeq = (opml \ "body" \ "outline" \ "outline")

    val fentries: Future[immutable.Seq[Entry]] = Future.sequence(outlines.dropRight(50).map { outline =>
      val rssUrl: String = outline \@ "xmlUrl"

      val entries: Future[Seq[Entry]] = getEntries(rssUrl)
      entries
    }).map(_.flatten)

    fentries.map { entries =>
      Ok(entries.map(_.toString).mkString("\n"))
    }
  }

  def getEntries(rssUrl: String): Future[Seq[Entry]] = {
    val fresponse: Future[WSResponse] = WS.url(rssUrl).get()

    fresponse.map { res: WSResponse =>
      val body = res.body
      val entriesXml: NodeSeq = (scala.xml.XML.loadString(body) \ "entry")

      val entries: Seq[Entry] = entriesXml.theSeq.map { entryXml: Node =>
        Entry(
          (entryXml \ "title").text,
          (entryXml \ "link" \@ "href"),
          (entryXml \ "author" \ "name").text,
          (entryXml \ "media:group" \ "media:thumbnail" \@ "url"),
          (entryXml \ "published").text,
          (entryXml \ "updated").text
        )
      }

      Console.println(rssUrl+" : "+entries.size)
      entries
    }
  }

  def indexHtml = Action {
    Ok(views.html.index(null))
  }
}

case class Entry(title: String,
                 url: String,
                 authorName: String,
                 thumbnail: String,
                 published: String,
                 updated: String)