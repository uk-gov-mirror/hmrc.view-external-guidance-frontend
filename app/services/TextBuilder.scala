/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import models.ocelot.Phrase
import models.ui._

import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object TextBuilder {
  private def placeholdersToItems(enM: List[Match], cyM: List[Match])(implicit urlMap: Map[String, String]): List[TextItem] = {

    // TODO Assume en and cy similar order for now
    def createLink(en: String, cy: String): Option[TextItem] =
      for{
        enMatch <- linkRegex.findFirstMatchIn(en)
        cyMatch <- linkRegex.findFirstMatchIn(cy)
      } yield {
        val enTxt = enMatch.group(1)
        val enLink = enMatch.group(2)
        val cyTxt = cyMatch.group(1)
        //val cyLink = cyMatch.group(2)
        // TODO also assume en and cy links are identical
        if (enLink.forall(_.isDigit)) {
          PageLink(urlMap(enLink), Text(enTxt, cyTxt))
        }
        else {
          HyperLink(enLink, Text(enTxt, cyTxt))
        }
      }

    enM.zip(cyM).map{ t =>
      val( en, cy) = t
      // TODO Assume en and cy similar order of placeholders for now
      (en.group(1), cy.group(1)) match {
        case ("link", "link") =>
          createLink(en.group(2), cy.group(2)).getOrElse{
            // if no matching link reconstitute original as Text
            Text(s"[link:${en.group(2)}]", s"[link:${cy.group(2)}]")
          }

        case ("bold", "bold") => Text(en.group(2), cy.group(2), true)

        // if en and cy dont match reconstitute original as Text
        case (x,y) => Text(s"[${x}:${en.group(2)}]", s"[${y}:${cy.group(2)}]")
      }
    }
  }

  private def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) =
    (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)

  private def textToTexts(enT: List[String], cyT: List[String]): List[Text] =
    enT.zip(cyT).map(t => Text(t._1, t._2))

  @tailrec
  private def mergeTextItems(txts: List[TextItem], links: List[TextItem], acc: Seq[TextItem]): Seq[TextItem] =
    (txts, links) match {
      case (Nil, Nil) =>  acc
      case (t :: txs, l :: lxs ) if t.isEmpty => mergeTextItems(txs, lxs, acc :+ l)
      case (t :: txs, l :: lxs ) => mergeTextItems(txs, lxs, (acc :+t) :+ l)
      case (t, Nil) => acc ++ t
      case (Nil, l) => acc ++ l
    }

  private def extractPlaceHolderAnnotation( matcher: Regex, txt: String ) : String = {

    // Find matches with regex
    val matches: List[Match] = matcher.findAllMatchIn(txt).toList

    // Find additional text components
    val texts: List[String] = matcher.split(txt).toList

    if (texts.size == 1 && matches.size == 0) {
      texts.head
    } else if (texts.size == 0 && matches.size == 1) {
      matches.head.group(1)
    } else {
      val matchTexts: List[String] = matches.map(_.group(1))

      val mergedTexts: List[String] = texts.zipAll(matchTexts, "", "") flatMap { case (a, b) => Seq(a, b) }

      mergedTexts.mkString
    }
  }

  def fromPhrase(txt: Phrase)(implicit urlMap: Map[String, String]): Seq[TextItem] = {

    val (enTexts, enMatches) = fromPattern(placeholderRegex, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(placeholderRegex, txt.langs(1))

    mergeTextItems(textToTexts(enTexts, cyTexts),
                   placeholdersToItems(enMatches, cyMatches),
                   Nil)
  }

  def extractBoldAndLinkTextAnnotation( txt: String ) : String = {

    extractLinkPlaceHolderAnnotation( extractBoldPlaceHolderAnnotation( txt ) )
  }

  def extractBoldPlaceHolderAnnotation( txt: String ) : String = {

    extractPlaceHolderAnnotation( boldTextRegex, txt )
  }

  def extractLinkPlaceHolderAnnotation( txt: String ) : String = {

    extractPlaceHolderAnnotation( linkRegex2, txt )
  }

  val placeholderRegex: Regex = """\[(link|bold):(.+?)\]""".r
  val linkRegex: Regex = """(.+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)""".r
  val linkRegex2: Regex = """\[link:(.*?):(http[s]?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
  val boldTextRegex: Regex = """\[bold:([^\]]*)\]""".r
}