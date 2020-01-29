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

import play.api.libs.functional.syntax._
import play.api.libs.json.{Reads, __}
import models.ocelot.stanzas._
import models.ocelot.{Page,Process}
import scala.annotation.tailrec


case class KeyedStanza(key:String, stanza:Stanza)

object PageBuilder {

  def buildPage(key: String, process: Process): Either[FlowError, Page] = {

    def pageUrl(values: List[Value]): Option[String] =
      values.filter(_.label.equals(PageUrlValueName.toString)) match {
        case Nil => None
        case x::xs => Some(x.value)
      }

    def isNewPageStanza(existingStanzas: Seq[KeyedStanza], stanza: ValueStanza): Boolean = existingStanzas.nonEmpty && pageUrl(stanza.values).isDefined

    @tailrec
    def collectStanzas(key: String, acc: Seq[KeyedStanza]): Either[FlowError, (Seq[KeyedStanza], Seq[String])] =
      process.flow.get(key) match {
        case Some(v:ValueStanza) if isNewPageStanza(acc, v) => Right((acc, acc.last.stanza.next))
        case Some(q:QuestionStanza) => Right((acc :+ KeyedStanza(key, q), q.next))
        case Some(EndStanza) => Right((acc :+ KeyedStanza(key, EndStanza), Nil))
        case Some(s:Stanza) if s.next.nonEmpty => collectStanzas( s.next.head, acc :+ KeyedStanza(key, s) )
        case Some(unknown) => Left(UnknownStanza(unknown))
        case None => Left(NoSuchPage(key))
      }

    collectStanzas(key, Nil) match {
      case Right((keyedStanzas, next)) =>
        keyedStanzas.head.stanza match {
          case v:ValueStanza if pageUrl(v.values).isDefined =>
              val stanzaMap = keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap
              Right(Page(keyedStanzas.head.key, pageUrl(v.values).get, stanzaMap, next))

          case _ =>
            Left(MissingPageUrlValueStanza(key))
        }

      case Left(err) => Left(err)
    }

  }

  def pages(process: Process, start: String = "start"): Either[FlowError, Seq[Page]] = {

    @tailrec
    def pagesByKeys(keys: Seq[String], acc: Seq[Page]): Either[FlowError, Seq[Page]] =
      keys match {
        case Nil => Right(acc)

        case key::xs if !acc.exists(_.id == key) =>
          buildPage(key, process) match {
            case Right(page) => pagesByKeys(page.next ++ xs, acc :+ page)
            case Left(err) => Left(err)
          }

        case key::xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil)
  }

}
