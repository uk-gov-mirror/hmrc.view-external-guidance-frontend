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
import models.ocelot.{Page, Process, Phrase}
import scala.annotation.tailrec

case class KeyedStanza(key: String, stanza: Stanza)

object PageBuilder {

  private def isNewPageStanza(existingStanzas: Seq[KeyedStanza], stanza: ValueStanza): Boolean =
    existingStanzas.nonEmpty && pageUrl(stanza.values).isDefined

  private def pageUrl(values: List[Value]): Option[String] =
    values.filter(_.label.equals(PageUrlValueName.toString)) match {
      case Nil => None
      case x :: _ if x.value.nonEmpty => Some(x.value)
      case _ => None
    }

  private def pageUrlUnique(url: String, existingPages:Seq[Page]): Boolean = !existingPages.exists(_.url == url)

  private def populateStanza(stanza: Stanza, process: Process): Either[FlowError, Stanza] = {
    def phrase(phraseIndex: Int): Either[FlowError, Phrase] =
      process.phraseOption(phraseIndex).map(Right(_)).getOrElse(Left(PhraseNotFound(phraseIndex)))

    @tailrec
    def phrases(indexes: Seq[Int], acc: Seq[Phrase]): Either[FlowError, Seq[Phrase]] =
      indexes match {
        case Nil => Right(acc)
        case index :: xs => phrase(index) match {
          case Right(phrase) => phrases(xs, acc :+ phrase)
          case Left(_) => Left(PhraseNotFound(index))
        }
      }

    stanza match {
      case q: QuestionStanza => phrases(q.text +: q.answers, Nil) match {
                                  case Right(texts) => Right(Question(q, texts.head, texts.tail))
                                  case Left(err) => Left(err)
                                }
      case i: InstructionStanza => phrase(i.text).fold(Left(_), text => Right(Instruction(i, text)))
      case c: CalloutStanza => phrase(c.text).fold(Left(_), text => Right(Callout(c,text)))
      case s: Stanza => Right(s)
    }
  }

  def buildPage(key: String, process: Process): Either[FlowError, Page] = {

    @tailrec
    def collectStanzas(key: String, acc: Seq[KeyedStanza]): Either[FlowError, (Seq[KeyedStanza], Seq[String])] =
      process.flow.get(key) match {
        case Some(s: Stanza) =>
          populateStanza(s, process) match {
            case Right(p) => p match {
              case v: ValueStanza if isNewPageStanza(acc, v) => Right((acc, acc.last.stanza.next))
              case v: ValueStanza => collectStanzas(v.next.head, acc :+ KeyedStanza(key, v))
              case i: Instruction => collectStanzas(i.next.head, acc :+ KeyedStanza(key, i))
              case c: Callout => collectStanzas(c.next.head, acc :+ KeyedStanza(key, c))
              case q: Question => Right((acc :+ KeyedStanza(key, q), q.next))
              case EndStanza => Right((acc :+ KeyedStanza(key, EndStanza), Nil))
              case unknown => Left(UnknownStanza(unknown))
            }
            case Left(err) => Left(err)
          }

        case None => Left(NoSuchPage(key))
      }

    collectStanzas(key, Nil) match {
      case Right((ks, next)) =>
        ks.head.stanza match {
          case v: ValueStanza if pageUrl(v.values).isDefined =>
            Right(Page(ks.head.key, pageUrl(v.values).get, ks.map(ks => (ks.key, ks.stanza)).toMap, next))
          case _ => Left(MissingPageUrlValueStanza(key))
        }

      case Left(err) => Left(err)
    }
  }

  def pages(process: Process, start: String = "start"): Either[FlowError, Seq[Page]] = {

    @tailrec
    def pagesByKeys(keys: Seq[String], acc: Seq[Page]): Either[FlowError, Seq[Page]] =
      keys match {
        case Nil => Right(acc)
        case key :: xs if !acc.exists(_.id == key) =>
          buildPage(key, process) match {
            case Right(page) if pageUrlUnique(page.url, acc) => pagesByKeys(page.next ++ xs, acc :+ page)
            case Right(page) => Left(DuplicatePageUrl(page.id, page.url))
            case Left(err) => Left(err)
          }
        case _ :: xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil)
  }

}
