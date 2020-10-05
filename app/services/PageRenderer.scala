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
import models.ocelot.stanzas.{EndStanza, Stanza, Question, Input, Evaluate}
import models.ocelot.{Page, Labels}

@Singleton
class PageRenderer @Inject() (appConfig: AppConfig) {

  def renderPage(page: Page, originalLabels: Labels): (Seq[Stanza], Labels) = {
    val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap

    @tailrec
    def evaluateStanzas(stanza: Stanza, labels: Labels, visualStanzas: Seq[Stanza]): (Seq[Stanza], Labels) =
      stanza match {
        case EndStanza => (visualStanzas :+ EndStanza, labels)
        case q: Question => (visualStanzas :+ q, labels)
        case i: Input => (visualStanzas :+ i, labels)
        case e: Stanza with Evaluate =>
          val (next, updatedLabels) = e.eval(labels)
          evaluateStanzas(stanzaMap(next), updatedLabels, visualStanzas)
        case s: Stanza => evaluateStanzas(stanzaMap(s.next(0)), labels, visualStanzas :+ s)
      }

    evaluateStanzas(stanzaMap(stanzaMap(page.id).next(0)), originalLabels, Nil)
  }

  def renderPagePostSubmit(postInputStanzaId: String, page: Page, originalLabels: Labels): (Seq[String], Labels) = {
    val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap

    // @tailrec
    // def findPostInputStanza(stanza: Stanza, labels: Labels): Option[Stanza] =
    //   stanza match {
    //     case EndStanza => (visualStanzas :+ EndStanza, labels)
    //     case q: Question => (visualStanzas :+ q, labels)
    //     case i: Input => (visualStanzas :+ i, labels)
    //     case e: Stanza with Evaluate =>
    //       val (next, updatedLabels) = e.eval(labels)
    //       evaluateStanzas(stanzaMap(next(0)), updatedLabels, visualStanzas)
    //     case s: Stanza => evaluateStanzas(stanzaMap(s.next(0)), labels, visualStanzas :+ s)
    //   }

    (Nil, originalLabels)
  }

}