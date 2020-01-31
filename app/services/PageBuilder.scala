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

import models.ocelot.stanzas._
import models.ocelot.{Page, Process}
import play.api.i18n.Lang

import scala.annotation.tailrec

case class KeyedStanza(key: String, stanza: Stanza)

object PageBuilder {

  val languageIndexMap: Map[String, Int] = Map("en" -> 0, "cy" -> 1)
  def languageIndex(lang: Lang): Int = languageIndexMap.get(lang.code).getOrElse(0) // Unknown language => English


  def buildPage(key: String, process: Process)(implicit lang:Lang): Either[FlowError, Page] = {

    implicit val langIndex: Int = languageIndex(lang)

    def phrase(index: Int): Either[FlowError, String] = process.phrase(index) match {
      case Some(text) => Right(text)
      case None => Left(PhraseNotFound(index))
    }

    def pageUrl(values: List[Value]): Option[String] =
      values.filter(_.label.equals(PageUrlValueName.toString)) match {
        case Nil => None
        case x :: _ => Some(x.value)
      }

    def isNewPageStanza(existingStanzas: Seq[KeyedStanza], stanza: ValueStanza): Boolean =
      existingStanzas.nonEmpty && pageUrl(stanza.values).isDefined

    def populateStanza(stanza: Stanza): Either[FlowError, Stanza] =
      stanza match {
        // case q: QuestionStanza =>
        //     val phrases: Seq[(Int, Option[String])] = (q.text :+ q.answers).map((i.text, process.phrase(i.text)))
        //     phrases.
        //       process.phrase(q.text).flatMap{ t =>
        //   Right(Instruction(i,t))).getOrElse(Left(PhraseNotFound(i.text)))
        // }
        case i: InstructionStanza => process.phrase(i.text).map(t => Right(Instruction(i,t))).getOrElse(Left(PhraseNotFound(i.text)))
        case c: CalloutStanza => process.phrase(c.text).map(t => Right(Callout(c,t))).getOrElse(Left(PhraseNotFound(c.text)))
        case s: Stanza => Right(s)
      }

    @tailrec
    def collectStanzas(key: String, acc: Seq[KeyedStanza]): Either[FlowError, (Seq[KeyedStanza], Seq[String])] =
      process.flow.get(key) match {
        case Some(s: Stanza) =>
          populateStanza(s) match {
            case Right(p) => p match {
              case v: ValueStanza if isNewPageStanza(acc, v) => Right((acc, acc.last.stanza.next))
              case v: ValueStanza => collectStanzas(v.next.head, acc :+ KeyedStanza(key, v))
              case i: Instruction => collectStanzas(i.next.head, acc :+ KeyedStanza(key, i))
              case c: Callout => collectStanzas(c.next.head, acc :+ KeyedStanza(key, c))
              case q: QuestionStanza => Right((acc :+ KeyedStanza(key, q), q.next))
              case EndStanza => Right((acc :+ KeyedStanza(key, EndStanza), Nil))
              case unknown => Left(UnknownStanza(unknown))
            }
            case Left(err) => Left(err)
          }
        case None => Left(NoSuchPage(key))
      }

    collectStanzas(key, Nil) match {
      case Right((keyedStanzas, next)) =>
        keyedStanzas.head.stanza match {
          case v: ValueStanza if pageUrl(v.values).isDefined =>
            val stanzaMap = keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap
            Right(Page(keyedStanzas.head.key, pageUrl(v.values).get, stanzaMap, next))

          case _ =>
            Left(MissingPageUrlValueStanza(key))
        }

      case Left(err) => Left(err)
    }

  }

  def pages(process: Process, start: String = "start")(implicit lang:Lang): Either[FlowError, Seq[Page]] = {

    @tailrec
    def pagesByKeys(keys: Seq[String], acc: Seq[Page]): Either[FlowError, Seq[Page]] =
      keys match {
        case Nil => Right(acc)

        case key :: xs if !acc.exists(_.id == key) =>
          buildPage(key, process) match {
            case Right(page) => pagesByKeys(page.next ++ xs, acc :+ page)
            case Left(err) => Left(err)
          }

        case _ :: xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil)
  }

}
