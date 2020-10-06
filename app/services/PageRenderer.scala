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

import config.AppConfig
import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import models.ocelot.stanzas.{EndStanza, Stanza, Evaluate, DataInput}
import models.ocelot.{Page, Labels}

@Singleton
class PageRenderer @Inject() (appConfig: AppConfig) {

  def renderPage(page: Page, originalLabels: Labels): (Seq[Stanza], Labels) = {
    val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap

    @tailrec
    def evaluateStanzas(stanza: Stanza, labels: Labels, visualStanzas: Seq[Stanza]): (Seq[Stanza], Labels) =
      stanza match {
        case EndStanza => (visualStanzas :+ EndStanza, labels)
        case s: Stanza with DataInput => (visualStanzas :+ s, labels)
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluateStanzas(stanzaMap(next), updatedLabels, visualStanzas)
        case s: Stanza => evaluateStanzas(stanzaMap(s.next(0)), labels, visualStanzas :+ s)
      }

    evaluateStanzas(stanzaMap(stanzaMap(page.id).next(0)), originalLabels, Nil)
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String): Option[(String, Labels)] = {
    val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap
    val pageIds = page.keyedStanzas.map( _.key)

    @tailrec
    def findDataInputStanza(stanzaId: String, seen: Seq[String]): Option[(Seq[String], DataInput)] =
      stanzaMap(stanzaId) match {
        case EndStanza => None
        case s: Stanza with DataInput  => Some((seen :+ stanzaId, s))
        case s: Stanza => findDataInputStanza(s.next(0), seen :+ stanzaId)
      }

    @tailrec
    def evaluatePostInputStanzas(next: String, labels: Labels, seen: Seq[String]): Option[(String, Labels)] =
      if (!pageIds.contains(next)) Some((next, labels))
      else if (seen.contains(next)) None
      else stanzaMap(next) match {
        case EndStanza => Some((next, labels))
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluatePostInputStanzas(next, updatedLabels, seen)
      }

    findDataInputStanza(stanzaMap(page.id).next(0), Nil)
      .fold[Option[(String, Labels)]](None){ case (seen, dataInputStanza) => {
          val (next, updatedLabels) = dataInputStanza.eval(answer, labels)
          evaluatePostInputStanzas(next, updatedLabels, seen)
        }
      }
  }

}