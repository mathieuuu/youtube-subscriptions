package controllers

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import play.api.Play.current
import play.api.libs.ws.{WS, WSResponse}
import play.api.mvc._

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
        val published = ZonedDateTime.parse((entryXml \ "published").text, dtf)
        val updated = ZonedDateTime.parse((entryXml \ "updated").text, dtf)

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

    val dtf: DateTimeFormatter = DateTimeFormatter.RFC_1123_DATE_TIME

    val entriesString = entries.map { e =>
      """<item>
                    <title><![CDATA[["""+ e.authorName +"""] """+ e.title +"""]]></title>
                    <link>""".stripMargin+e.url+"""</link><description></description>
                    <enclosure type="image/jpg" url=""""+e.thumbnail+""""/>
                    <pubDate>"""+dtf.format(e.published)+"""</pubDate>
         </item>
      """.stripMargin
    }.mkString("")

    val xml: String = """<?xml version="1.0" encoding="UTF-8"?>
      <rss version="2.0">
        <channel>
          <title>Mes abos YouTube</title>
          <link>https://www.youtube.com/feed/subscriptions</link>
          <description>Mes abos YouTube</description>
          <language>fr-FR</language>
          """ + entriesString + """
        </channel>
      </rss>
      """

    scala.xml.XML.loadString(xml)
  }

  def indexHtml = Action {
    Ok(views.html.index(null))
  }
}

case class Entry(title: String,
                 url: String,
                 authorName: String,
                 thumbnail: String,
                 published: ZonedDateTime,
                 updated: ZonedDateTime)

