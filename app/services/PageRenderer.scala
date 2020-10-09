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

import javax.inject.{Inject, Singleton}
import scala.annotation.tailrec
import models.ocelot.stanzas.{EndStanza, Stanza, Evaluate, DataInput}
import models.ocelot.{Page, Labels}

@Singleton
class PageRenderer @Inject() () {

  @tailrec
   private def evaluateStanzas(stanzaId: String, labels: Labels, visualStanzas: Seq[Stanza], seen: Seq[String])
                              (implicit stanzaMap: Map[String, Stanza], ids: Seq[String]): (Seq[Stanza], Labels, Seq[String], String, Option[DataInput]) =
    if (!ids.contains(stanzaId)) (visualStanzas, labels, seen, stanzaId, None)
    else
    stanzaMap(stanzaId) match {
      case EndStanza => (visualStanzas :+ EndStanza, labels, seen :+ stanzaId, stanzaId, None)
      case s: Stanza with DataInput => (visualStanzas :+ s, labels, seen :+ stanzaId, stanzaId, Some(s))
      case s: Stanza with Evaluate =>
        val (next, updatedLabels) = s.eval(labels)
        evaluateStanzas(next, updatedLabels, visualStanzas, seen :+ stanzaId)
      case s: Stanza => evaluateStanzas(s.next(0), labels, visualStanzas :+ s, seen :+ stanzaId)
    }

  def renderPage(page: Page, labels: Labels): (Seq[Stanza], Labels, Option[DataInput]) = {
    implicit val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap
    implicit val ids = page.keyedStanzas.map(_.key)

    val (visualStanzas, newLabels, _, _, optionalInput) = evaluateStanzas(stanzaMap(page.id).next(0), labels, Nil, Nil)

    (visualStanzas, newLabels, optionalInput)
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String): Option[(String, Labels)] = {
    implicit val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap
    implicit val ids = page.keyedStanzas.map( _.key)

    @tailrec
    def evaluatePostInputStanzas(next: String, labels: Labels, seen: Seq[String]): Option[(String, Labels)] =
      if (!ids.contains(next)) Some((next, labels))
      else if (seen.contains(next)) None
      else stanzaMap(next) match {
        case EndStanza => Some((next, labels))
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluatePostInputStanzas(next, updatedLabels, seen)
      }

    val (visual, newLabels, seen, nextPageId, optionalInput) = evaluateStanzas(stanzaMap(page.id).next(0), labels, Nil, Nil)
    optionalInput.fold[Option[(String, Labels)]](Some((nextPageId, newLabels))){dataInputStanza =>
      dataInputStanza.eval(answer, newLabels).flatMap{case (next, postInputLabels) =>
        evaluatePostInputStanzas(next, postInputLabels, seen)
      }
    }
  }

}