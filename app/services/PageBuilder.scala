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
import models.Page
import models.ocelot.stanzas._
import models.ocelot.Process
import scala.annotation.tailrec


case class KeyedStanza(key:String, stanza:Stanza)

object PageBuilder {

  def buildPage( key: String, process: Process): Option[Page] = {

    @tailrec
    def collectStanzas(key: String, acc: Seq[KeyedStanza]): (Seq[KeyedStanza], Seq[String]) =
      process.flow.get(key) match {
        case Some(i:InstructionStanza) => collectStanzas( i.next.head, acc :+ KeyedStanza(key, i) )
        case Some(c:CalloutStanza) => collectStanzas( c.next.head, acc :+ KeyedStanza(key, c) )
        case Some(v:ValueStanza) => collectStanzas( v.next.head, acc :+ KeyedStanza(key, v) )
        case Some(q:QuestionStanza) => (acc :+ KeyedStanza(key, q), q.next)
        case Some(EndStanza) => (acc :+ KeyedStanza(key, EndStanza), Nil)
        case _ => (Nil, Nil)
      }

    collectStanzas(key, Nil) match {
      case (Nil, _) => None
      case (keyedStanzas, next) => Some(Page( keyedStanzas.head.key, keyedStanzas.map( _.stanza), next))
    }

  }

  def pages(process: Process, start: String = "start"):Seq[Page] = {
    @tailrec
    def pagesByKeys(keys: Seq[String], acc: Seq[Page]): Seq[Page] =
      keys match {
        case Nil => acc
        case key::xs if !acc.exists(_.id == key) =>
          buildPage(key, process) match {
            case Some(page) => pagesByKeys(page.next ++ xs, acc :+ page)
            case None => Nil
          }
        case key::xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil)
  }

}