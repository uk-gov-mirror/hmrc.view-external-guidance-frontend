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

import models.ocelot.stanzas.{Instruction, InstructionGroup, ValueStanza, EndStanza, Callout, Title, SubTitle, Lede, Error, Section}
import models.ocelot.Phrase
import play.api.Logger
import models.ocelot.stanzas.{Question => OcelotQuestion}
import models.ocelot.{Page => OcelotPage, Link => OcelotLink}
import models.ui._

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
          case c: Callout => acc ++ fromCallout(c, formData)
            
          case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _) if dest.forall(_.isDigit) =>
            acc ++ Seq(Paragraph(Text.linkText(stanzaIdToUrlMap(dest), txt.langs), window))

          case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _) =>
            acc ++ Seq(Paragraph(Text.linkText(dest, txt.langs), window))

          case Instruction(txt, _, _, _) =>
            acc ++ Seq(Paragraph(TextBuilder.fromPhrase(txt)))

          case ig: InstructionGroup => acc :+ fromInstructionGroup(ig)

          case q: OcelotQuestion => Seq(fromQuestion(q, formData, acc))

          case ValueStanza(_, _, _) => acc
          case EndStanza => acc
        }
      }
    )

  private def fromQuestion(q: OcelotQuestion, formData: Option[FormData], components: Seq[UIComponent])
                          (implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    val answers = (q.answers zip q.next).map { t =>
      val (phrase, stanzaId) = t
      val (answer, hint) = TextBuilder.answerTextWithOptionalHint(phrase)
      Answer(answer, hint, stanzaIdToUrlMap(stanzaId))
    }
    // Split out an Error callouts from body components
    val (errorCallouts, uiElements) = components.partition{
                                        case msg: ErrorCallout => true
                                        case _             => false
                                      }

    // Build error messages if required
    val errorMsgs = formData.fold(Seq.empty[ErrorMsg]){uiData =>
      // TODO Currently radio button forms only => single error, no entry made
      (uiData.errors zip errorCallouts).map{t =>
        val (formError, callout) = t
        ErrorMsg(formError.key, callout.text)
      }
    }

    Question(Text(q.text.langs), uiElements, answers, errorMsgs)
  }

  private def fromCallout(c: Callout, formData: Option[FormData])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    c.noteType match {
      case Title => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case SubTitle => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case Section => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case Lede => Seq(Paragraph(TextBuilder.fromPhrase(c.text), true))
      // Ignore error messages if no errors exist within form data
      case Error if formData.isEmpty || formData.get.errors.isEmpty => Seq.empty
      case Error => Seq(ErrorCallout(TextBuilder.fromPhrase(c.text)))
    }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit stanzaIdToUrlMap: Map[String, String]): BulletPointList = {

    def createRemainingItems(leadingEn: String, leadingCy: String, remainder: Seq[Instruction])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[Text] =
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

    // Determine first bullet point
    val firstBpEn: String =
      insGroup.group.head.text.langs(0).substring(leadingEn.length, insGroup.group.head.text.langs(0).length).trim

    val firstBpCy: String = insGroup.group.head.text
      .langs(1)
      .substring(leadingCy.length, insGroup.group.head.text.langs(1).length)
      .trim

    val firstBulletPointItems: Text = TextBuilder.fromPhrase(Phrase(firstBpEn, firstBpCy))

    // Process remaining bullet points
    val remainder: Seq[Text] = createRemainingItems(leadingEn, leadingCy, insGroup.group.drop(1))

    BulletPointList(TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)), firstBulletPointItems +: remainder)
  }

}
