package controllers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import play.api.mvc._
import play.api.libs.ws.{WS, WSResponse}
import play.api.Play.current
import play.api.libs.json.Json

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.xml.{Elem, Node, NodeSeq}

object Application extends Controller {

  def index = Action.async {

    val opml: Elem = scala.xml.XML.loadFile("conf/subscription_manager.xml")
    val outlines: NodeSeq = (opml \ "body" \ "outline" \ "outline")

    val fentries: Future[immutable.Seq[Entry]] = Future.sequence(outlines.map { outline =>
      val rssUrl: String = outline \@ "xmlUrl"

      val entries: Future[Seq[Entry]] = getEntries(rssUrl)
      entries
    }).map(_.flatten)

    fentries.map { entries =>
      val displayedEntries: immutable.Seq[Entry] = entries.sortWith((a, b) => a.published.isAfter(b.published)).take(50)

      val xml = makeRss(displayedEntries)

      Ok(xml)
    }
  }

  def getEntries(rssUrl: String): Future[Seq[Entry]] = {
    val fresponse: Future[WSResponse] = WS.url(rssUrl).get()

    fresponse.map { res: WSResponse =>
      val body = res.body
      val entriesXml: NodeSeq = (scala.xml.XML.loadString(body) \ "entry")

      val entries: Seq[Entry] = entriesXml.theSeq.map { entryXml: Node =>

        val dtf: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
        val published = LocalDateTime.parse((entryXml \ "published").text, dtf)
        val updated = LocalDateTime.parse((entryXml \ "updated").text, dtf)

        Entry(
          (entryXml \ "title").text,
          (entryXml \ "link" \@ "href"),
          (entryXml \ "author" \ "name").text,
          (entryXml \ "group" \ "thumbnail" \@ "url"),
          published,
          updated
        )
      }

      Console.println(rssUrl+" : "+entries.size)
      entries
    }.recover {
      case NonFatal(e) => {
        Console.println("Following error occured while parsing "+rssUrl+" : "+e.getMessage)
        e.printStackTrace()
        Seq.empty
      }
    }
  }

  def makeRss(entries: Seq[Entry]) = {

    val dtf: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

    val entriesString = entries.map { e =>
      """<item>
                    <title>["""+ e.authorName +"""] """+ e.title +"""</title>
                    <link>"""+e.url+"""</link>
                    <description>"""+e.title+"""</description>
                    <enclosure type="image/jpg" url=""""+e.thumbnail+""""/>
                  </item>
      """.stripMargin
    }.mkString("")

    val xml: String = """<rss version="2.0">
        <channel>
          <title>Mes abos YouTube</title>
          <link>https://www.youtube.com/feed/subscriptions</link>
          <description>Mes abos YouTube</description>à
          <language>fr-FR</language>
          """ + entriesString + """
        </channel>
      </rss>
      """

    xml
  }

  def indexHtml = Action {
    Ok(views.html.index(null))
  }
}

case class Entry(title: String,
                 url: String,
                 authorName: String,
                 thumbnail: String,
                 published: LocalDateTime,
                 updated: LocalDateTime)

