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
import models.ocelot.{Process, Page}
import scala.annotation.tailrec

case class KeyedStanza(key: String, stanza: Stanza)

object PageBuilder extends ProcessPopulation {

  private def isNewPageStanza(existingStanzas: Seq[KeyedStanza], stanza: ValueStanza): Boolean =
    existingStanzas.nonEmpty && pageUrl(stanza.values).isDefined

  private def pageUrl(values: List[Value]): Option[String] =
    values.filter(_.label.equals(PageUrlValueName.toString)) match {
      case Nil => None
      case x :: _ if x.value.nonEmpty => Some(x.value)
      case _ => None
    }

  private def pageUrlUnique(url: String, existingPages:Seq[Page]): Boolean = !existingPages.exists(_.url == url)

  def buildPage(key: String, process: Process): Either[FlowError, Page] = {

    @tailrec
    def collectStanzas(key: String,
                       acc: Seq[KeyedStanza],
                       linkedPageAcc: Seq[String]): Either[FlowError, (Seq[KeyedStanza], Seq[String], Seq[String])] =
      stanza(key, process) match {
        case Right(v: ValueStanza) if isNewPageStanza(acc, v) => Right((acc, acc.last.stanza.next, linkedPageAcc))
        case Right(v: ValueStanza) => collectStanzas(v.next.head, acc :+ KeyedStanza(key, v), linkedPageAcc)
        case Right(i: Instruction) => collectStanzas(i.next.head, acc :+ KeyedStanza(key, i), linkedPageAcc)
        case Right(c: Callout) => collectStanzas(c.next.head, acc :+ KeyedStanza(key, c), linkedPageAcc)
        case Right(q: Question) => Right((acc :+ KeyedStanza(key, q), q.next, linkedPageAcc))
        case Right(EndStanza) => Right((acc :+ KeyedStanza(key, EndStanza), Nil, Nil))
        case Right(unknown) => Left(UnknownStanzaType(unknown))

        case Left(err) => Left(err)
      }

    collectStanzas(key, Nil, Nil) match {
      case Right((ks, next, linked)) =>
        ks.head.stanza match {
          case v: ValueStanza if pageUrl(v.values).isDefined =>
            Right(Page(ks.head.key, pageUrl(v.values).get, ks.map(_.stanza), next, linked))
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
            case Right(page) if pageUrlUnique(page.url, acc) =>
              pagesByKeys(page.next ++ xs ++ page.linked, acc :+ page)
            case Right(page) => Left(DuplicatePageUrl(page.id, page.url))
            case Left(err) => Left(err)
          }
        case _ :: xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil)
  }

}
