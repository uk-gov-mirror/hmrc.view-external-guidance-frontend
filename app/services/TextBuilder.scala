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

  private def placeholdersToItems(enM: List[Match], cyM: List[Match])(implicit urlMap: Map[String, String]): List[TextItem] =
    enM.zip(cyM).map{ t =>
      val( en, cy) = t
      // TODO Assume en and cy similar order of placeholders for now
      (Option(en.group(1)), Option(cy.group(1))) match {
        case (Some(enBold), Some(cyBold)) =>
          Text(enBold, cyBold, true)

        case (None, None) =>
          val enTxt = en.group(2)
          val enDest = en.group(3)
          val cyTxt = cy.group(2)
          //val cyDest = cy.group(3)
          // TODO also assume en and cy links are identical
          if (enDest.forall(_.isDigit)) {
            PageLink(urlMap(enDest), Text(enTxt, cyTxt))
          }
          else {
            HyperLink(enDest, Text(enTxt, cyTxt))
          }

        // if en and cy dont match reconstitute original as Text
        case (Some(enBold),None) => Text(enBold, cy.group(2), true)
        case (None, Some(cyBold)) => Text(en.group(2), cyBold, true)
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

  def wordsToDisplayAsList(str: String): List[String] = {
    val isEmpty:String => Boolean = _.isEmpty

    val (txts, matches) = fromPattern(placeholdersPattern, str)
    val matchTexts = matches.map(m => Option(m.group(1)).getOrElse(m.group(2)))
    merge(txts, matchTexts, Nil, isEmpty).mkString(" ").split(" +").toList
  }

  def wordsToDisplay(str: String): String = wordsToDisplayAsList(str).mkString(" ")

  def fromPhrase(txt: Phrase)(implicit urlMap: Map[String, String]): Seq[TextItem] = {
    val isEmpty: TextItem => Boolean = _.isEmpty

    val (enTexts, enMatches) = fromPattern(placeholdersPattern, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(placeholdersPattern, txt.langs(1))

    merge(textToTexts(enTexts, cyTexts), placeholdersToItems(enMatches, cyMatches), Nil, isEmpty)
  }

  val placeholdersPattern: Regex = """\[bold:([^\]]+)\]|\[link:([^\]]+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
}