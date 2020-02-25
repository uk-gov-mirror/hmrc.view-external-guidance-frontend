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

import models.ocelot.stanzas.{Instruction,ValueStanza,EndStanza,Callout,Title,SubTitle,Lede,Error}
import models.ocelot.Link
import models.ui._
// import models.ocelot.stanzas._
// import models.ocelot.Link
// import models.ui.{Text,H1,H2,H3,Paragraph,Page,UIComponent,HyperLink}

object UIBuilder {

  private def fromCallout(c: Callout): UIComponent =
    c.noteType match {
      case Title => H1(Text(c.text.langs))
      case SubTitle => H2(Text(c.text.langs))
      case Lede => Paragraph(Seq(Text(c.text.langs)), true)
      case Error => H3(Text(c.text.langs)) // TODO
  }

  def fromStanzaPage(pge: models.ocelot.Page)(implicit stanzaIdToUrlMap: Map[String, String]): Page =
    Page(
      pge.url,
      pge.stanzas.foldLeft(Seq[UIComponent]()){(acc, stanza) =>
        stanza match {
          case c: Callout => acc ++ Seq(fromCallout(c))

          case Instruction(txt,_,Some(Link(id,dest,_,window)),_) if dest.forall(_.isDigit) =>
            acc ++ Seq(Paragraph(Seq(HyperLink(stanzaIdToUrlMap(dest), Text(txt.langs), window))))

          case Instruction(txt,_,Some(Link(id,dest,_,window)),_) =>
            acc ++ Seq(Paragraph(Seq(HyperLink(dest, Text(txt.langs), window))))

          case Instruction(txt,_,_,_) =>
            acc ++ Seq(Paragraph(TextBuilder.fromPhrase(txt)))

          case models.ocelot.stanzas.Question(txt,ans,next,stack) =>
            val answers = (ans zip next).map{ t =>
              val (phrase, stanzaId) = t
              Answer(Text(phrase.langs), None, stanzaId)
            }
            Seq(Question(Text(txt.langs), acc, answers))

          case ValueStanza(_,_,_) => acc
          case EndStanza => acc
        }
      }
    )

  def pages(stanzaPages: Seq[models.ocelot.Page]): Map[String, Page] = {
    val stanzaIdToUrlMap = stanzaPages.map(p => (p.id, p.url)).toMap
    stanzaPages.map(p => (p.url, fromStanzaPage(p)(stanzaIdToUrlMap))).toMap
  }
}

