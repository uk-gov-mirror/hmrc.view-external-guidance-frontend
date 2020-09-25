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

import models.ocelot.{Link => OcelotLink, Phrase, Process}
import models.ui._
import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object TextBuilder {

  private val answerHintPattern: Regex = """\[hint:([^\]]+)\]""".r

  private object PL { // All the placeholder matching in one place
    val regex: Regex = s"\\[label:([A-Za-z0-9\\s\\-]+)\\]|\\[bold:([^\\]]+)\\]|\\[link(-same|-tab)?:([^\\]]+?):(\\d+|${Process.StartStanzaId}|https?:[a-zA-Z0-9\\/\\.\\-\\?_\\.=&]+)\\]".r
    def labelNameOpt(m: Match): Option[String] = Option(m.group(1))
    def boldTextOpt(m: Match): Option[String] = Option(m.group(2))
    def linkTypeOpt(m: Match): Option[String] = Option(m.group(3))
    def linkText(m: Match): String = m.group(4)
    def linkTextOpt(m: Match): Option[String] = Option(linkText(m))
    def linkDest(m: Match): String = m.group(5)
  }

  private def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) =
    (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)

  private def placeholdersToItems(matches: List[Match])(implicit urlMap: Map[String, String]): List[TextItem] =
    matches.map { m =>
      PL.labelNameOpt(m).fold[TextItem]({
        PL.boldTextOpt(m).fold[TextItem]({
          val window: Boolean = PL.linkTypeOpt(m).fold(false)(modifier => modifier == "-tab")
          val dest: String = if (OcelotLink.isLinkableStanzaId(PL.linkDest(m))) urlMap(PL.linkDest(m)) else PL.linkDest(m)
          Link(dest, PL.linkText(m), window)
        })(txt => Words(txt, true))
      })(labelName => LabelRef(labelName))
    }

  def fromPhrase(txt: Phrase)(implicit urlMap: Map[String, String]): Text = {
    val isEmpty: TextItem => Boolean = _.isEmpty

    val (enTexts, enMatches) = fromPattern(PL.regex, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(PL.regex, txt.langs(1))

    val en = merge(enTexts.map(Words(_)), placeholdersToItems(enMatches), Nil, isEmpty)
    val cy = merge(cyTexts.map(Words(_)), placeholdersToItems(cyMatches), Nil, isEmpty)
    Text(en, cy)
  }

  // Parses a string potentially containing a hint pattern[hint:<Text Hint>]
  // The text before the first hint (if any) and if so the first hint will be
  // returned. All subsequent text and hints will be ignored and lost
  def singleTextWithOptionalHint(txt: Phrase): (Text, Option[Text]) = {
    val (enTexts, enMatches) = fromPattern(answerHintPattern, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(answerHintPattern, txt.langs(1))

    val enHint = enMatches.headOption.map(enM => enM.group(1))
    val cyHint = cyMatches.headOption.map(cyM => cyM.group(1))
    val hint = enHint.map(en => Text(en, cyHint.getOrElse("")))
    (Text(enTexts.head, cyTexts.head), hint)
  }

  @tailrec
  def merge[A, B](txts: List[A], links: List[A], acc: Seq[A], isEmpty: A => Boolean): Seq[A] =
    (txts, links) match {
      case (Nil, Nil) => acc
      case (t :: txs, l :: lxs) if isEmpty(t) => merge(txs, lxs, acc :+ l, isEmpty)
      case (t :: txs, l :: lxs) => merge(txs, lxs, (acc :+ t) :+ l, isEmpty)
      case (t, Nil) => acc ++ t
      case (Nil, l) => acc ++ l
    }

  //
  // Following used by BulletPointBuilder
  //
  def placeholderMatchText(m: Match): String = PL.boldTextOpt(m).getOrElse(PL.linkTextOpt(m).getOrElse(""))
  def placeholderTxtsAndMatches(text: String): (List[String], List[Match]) = fromPattern(PL.regex, text)
  def flattenPlaceholders(text: String): Seq[String] = {
    val (txts, matches) = fromPattern(PL.regex, text)
    merge[String, String](txts, matches.map(m => PL.boldTextOpt(m).fold(PL.linkTextOpt(m).getOrElse(""))(v => v)), Nil, _.isEmpty).filterNot(_.isEmpty)
  }
}
