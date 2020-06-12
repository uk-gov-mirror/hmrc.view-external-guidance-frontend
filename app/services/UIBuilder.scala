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

import javax.inject.Singleton

import models.ocelot.stanzas.{Instruction, InstructionGroup, ValueStanza, PageStanza, EndStanza, Callout, Title, SubTitle, Lede, Error, Section}
import models.ocelot.Phrase
import play.api.Logger
import models.ocelot.stanzas.{Question => OcelotQuestion}
import models.ocelot.{Page => OcelotPage, Link => OcelotLink}
import models.ui._
import scala.annotation.tailrec

@Singleton
class UIBuilder {
  val logger = Logger(getClass)

  def pages(stanzaPages: Seq[OcelotPage], formData: Option[FormData] = None)(implicit stanzaIdToUrlMap: Map[String, String]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p, formData)(stanzaIdToUrlMap))).toMap

  def fromStanzaPage(pge: OcelotPage, formData: Option[FormData] = None)(implicit stanzaIdToUrlMap: Map[String, String]): Page =
    Page(
      pge.url,
      pge.stanzas.foldLeft(Seq[UIComponent]()) { (acc, stanza) =>
        stanza match {
          case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _) if OcelotLink.isLinkableStanzaId(dest) =>
            acc ++ Seq(Paragraph(Text.link(stanzaIdToUrlMap(dest), txt.langs), window))
          case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _) => acc ++ Seq(Paragraph(Text.link(dest, txt.langs), window))
          case Instruction(txt, _, _, _) => acc ++ Seq(Paragraph(TextBuilder.fromPhrase(txt)))

          case ig: InstructionGroup => acc :+ fromInstructionGroup(ig)
          case c: Callout => acc ++ fromCallout(c, formData)
          case q: OcelotQuestion => Seq(fromQuestion(q, formData, acc))
          case PageStanza(_, _, _) => acc
          case ValueStanza(_, _, _) => acc
          case EndStanza => acc
        }
      }
    )

  private def fromQuestion(q: OcelotQuestion, formData: Option[FormData], components: Seq[UIComponent])(
      implicit stanzaIdToUrlMap: Map[String, String]
  ): UIComponent = {
    val answers = (q.answers zip q.next).map { t =>
      val (phrase, stanzaId) = t
      val (answer, hint) = TextBuilder.singleTextWithOptionalHint(phrase)
      Answer(answer, hint, stanzaIdToUrlMap(stanzaId))
    }
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (question, hint) = TextBuilder.singleTextWithOptionalHint(q.text)
    Question(question, hint, uiElements, answers, errorMsgs)
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromCallout(c: Callout, formData: Option[FormData])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    c.noteType match {
      case Title => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case SubTitle => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case Section => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case Lede => Seq(Paragraph(TextBuilder.fromPhrase(c.text), true))
      // Ignore error messages if no errors exist within form data
      case Error if formData.isEmpty || formData.get.errors.isEmpty => Seq.empty
      case Error =>
        // TODO this should allocate the messages to errors found within the formData
        // as this linking of messages to form ids has not been resolved, Currently
        // this code will allocate all ErrorMsg elements to the only current error
        // which is error.required
        formData
          .map { data =>
            data.errors.map { err =>
              ErrorMsg(err.key, TextBuilder.fromPhrase(c.text))
            }
          }
          .getOrElse(Seq.empty)
    }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit stanzaIdToUrlMap: Map[String, String]): BulletPointList = {

    def createBulletPointItems(leadingEn: String, leadingCy: String, remainder: Seq[Instruction])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[Text] =
      remainder.map { instruction =>
        val bulletPointEnglish: String = instruction.text
          .langs(0)
          .substring(leadingEn.length, instruction.text.langs(0).length)
          .trim

        val bulletPointWelsh: String = instruction.text
          .langs(1)
          .substring(leadingCy.length, instruction.text.langs(1).length)
          .trim

        TextBuilder.fromPhrase(Phrase(bulletPointEnglish, bulletPointWelsh))
      }

    val leadingEn: String = BulletPointBuilder.determineMatchedLeadingText(
      insGroup.group.head.text.langs(0),
      insGroup.group(1).text.langs(0)
    )

    val leadingCy: String = BulletPointBuilder.determineMatchedLeadingText(
      insGroup.group.head.text.langs(1),
      insGroup.group(1).text.langs(1)
    )

    // Process bullet points
    val bulletPointListItems: Seq[Text] = createBulletPointItems(leadingEn, leadingCy, insGroup.group)

    BulletPointList(TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)), bulletPointListItems)
  }

}
