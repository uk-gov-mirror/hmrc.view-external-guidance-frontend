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
  private def merge[A,B](txts: List[A], links: List[A], acc: Seq[A], isEmpty:A => Boolean): Seq[A] =
    (txts, links) match {
      case (Nil, Nil) =>  acc
      case (t :: txs, l :: lxs ) if isEmpty(t) => merge(txs, lxs, acc :+ l, isEmpty)
      case (t :: txs, l :: lxs ) => merge(txs, lxs, (acc :+t) :+ l, isEmpty)
      case (t, Nil) => acc ++ t
      case (Nil, l) => acc ++ l
    }

  def wordsToDisplayInPlaceholderString(str: String): Seq[String] = {
    val isEmpty:String => Boolean = _.isEmpty

    val (txts, matches) = fromPattern(placeholdersPattern, str)
    val matchTexts = matches.map(m => Option(m.group(1)).getOrElse(m.group(2)))
    merge(txts, matchTexts, Nil, isEmpty).mkString(" ").split(" +")
  }

  def fromPhrase(txt: Phrase)(implicit urlMap: Map[String, String]): Seq[TextItem] = {
    val isEmpty: TextItem => Boolean = _.isEmpty

    val (enTexts, enMatches) = fromPattern(placeholderRegex, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(placeholderRegex, txt.langs(1))

    merge(textToTexts(enTexts, cyTexts), placeholdersToItems(enMatches, cyMatches), Nil, isEmpty)
  }

  val placeholdersPattern = """\[bold:([^\]]+)\]|\[link:([^\]]+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
  val placeholderRegex = """\[(link|bold):(.+?)\]""".r
  val linkRegex = """(.+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)""".r
}