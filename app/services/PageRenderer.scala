/*
 * Copyright 2021 HM Revenue & Customs
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
import models.ocelot.stanzas.{EndStanza, VisualStanza, Stanza, Evaluate, DataInput}
import models.ocelot.{Page, Labels}

@Singleton
class PageRenderer @Inject() () {

  def renderPage(page: Page, labels: Labels): (Seq[VisualStanza], Labels, Option[DataInput]) = {
    implicit val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap

    val (visualStanzas, newLabels, _, _, optionalInput) = evaluateStanzas(stanzaMap(page.id).next.head, labels)

    (visualStanzas, newLabels, optionalInput)
  }

  def renderPagePostSubmit(page: Page, labels: Labels, answer: String): (Option[String], Labels) = {
    implicit val stanzaMap = page.keyedStanzas.map(ks => (ks.key, ks.stanza)).toMap

    @tailrec
    def evaluatePostInputStanzas(next: String, labels: Labels, seen: Seq[String]): (Option[String], Labels) =
      if (next == page.id) (None, labels)                     // next indicates current page
      else stanzaMap.get(next) match {
        case None => (Some(next), labels)
        case Some(_) if seen.contains(next) => (None, labels) // Legacy: Interpret redirect to stanza prior to input as current page
        case Some(s) => s match {
          case EndStanza => (Some(next), labels)
          case s: Stanza with Evaluate =>
            val (next, updatedLabels) = s.eval(labels)
            evaluatePostInputStanzas(next, updatedLabels, seen)
        }
      }

    val (visual, newLabels, seen, nextPageId, optionalInput) = evaluateStanzas(stanzaMap(page.id).next.head, labels, Nil, Nil)
    optionalInput.fold[(Option[String], Labels)]((Some(nextPageId), newLabels)){dataInputStanza =>
      val (next, postInputLabels) = dataInputStanza.eval(answer, newLabels)
      next.fold[(Option[String], Labels)]((None, postInputLabels))(evaluatePostInputStanzas(_, postInputLabels, seen))
    }
  }

  @tailrec
   private def evaluateStanzas(stanzaId: String, labels: Labels, visualStanzas: Seq[VisualStanza] = Nil, seen: Seq[String] = Nil)
                              (implicit stanzaMap: Map[String, Stanza]): (Seq[VisualStanza], Labels, Seq[String], String, Option[DataInput]) =
    stanzaMap.get(stanzaId) match {
      case None => (visualStanzas, labels, seen, stanzaId, None)
      case Some(s) => s match {
        case EndStanza => (visualStanzas, labels, seen :+ stanzaId, stanzaId, None)
        case s: VisualStanza with DataInput => (visualStanzas :+ s, labels, seen :+ stanzaId, stanzaId, Some(s))
        case s: Stanza with Evaluate =>
          val (next, updatedLabels) = s.eval(labels)
          evaluateStanzas(next, updatedLabels, visualStanzas, seen :+ stanzaId)
        case s: VisualStanza => evaluateStanzas(s.next.head, labels, visualStanzas :+ s, seen :+ stanzaId)
      }
    }
}