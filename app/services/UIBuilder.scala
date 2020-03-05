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

import models.ocelot.stanzas.{Instruction, InstructionGroup, ValueStanza, EndStanza, Callout, Title, SubTitle, Lede, Error, Section}
import models.ocelot.Phrase
import models.ui._

object UIBuilder {

  def pages(stanzaPages: Seq[models.ocelot.Page])(implicit stanzaIdToUrlMap: Map[String, String]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p)(stanzaIdToUrlMap))).toMap

  def fromStanzaPage(pge: models.ocelot.Page)(implicit stanzaIdToUrlMap: Map[String, String]): Page =
    Page(
      pge.url,
      pge.stanzas.foldLeft(Seq[UIComponent]()) { (acc, stanza) =>
        stanza match {
          case c: Callout => acc ++ Seq(fromCallout(c))

          case Instruction(txt, _, Some(models.ocelot.Link(id, dest, _, window)), _) if dest.forall(_.isDigit) =>
            val linkText = Text(Link(stanzaIdToUrlMap(dest), txt.langs(0)),
                                Link(stanzaIdToUrlMap(dest), txt.langs(1)))
            acc ++ Seq(Paragraph(linkText, window))

          case Instruction(txt, _, Some(models.ocelot.Link(id, dest, _, window)), _) =>
            val linkText = Text(Link(dest, txt.langs(0)), Link(dest, txt.langs(1)))
            acc ++ Seq(Paragraph(linkText, window))

          case Instruction(txt, _, _, _) =>
            acc ++ Seq(Paragraph(TextBuilder.fromPhrase(txt)))

          case ig: InstructionGroup => acc :+ fromInstructionGroup(ig)

          case models.ocelot.stanzas.Question(txt, ans, next, stack) =>
            val answers = (ans zip next).map { t =>
              val (phrase, stanzaId) = t
              val (answer, hint) = TextBuilder.answerTextWithOptionalHint(phrase)
              Answer(answer, hint, stanzaIdToUrlMap(stanzaId))
            }
            Seq(Question(Text(txt.langs), acc, answers))

          case ValueStanza(_, _, _) => acc
          case EndStanza => acc
        }
      }
    )

  private def fromCallout(c: Callout): UIComponent =
    c.noteType match {
      case Title => H1(Text(c.text.langs))
      case SubTitle => H2(Text(c.text.langs))
      case Section => H3(Text(c.text.langs))
      case Lede => Paragraph(Text(c.text.langs), true)
      case Error => H3(Text(c.text.langs)) // TODO
    }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit stanzaIdToUrlMap: Map[String, String]): BulletPointList = {

    def createRemainingItems(leadingEn: String,
                                     leadingCy: String,
                                     remainder: Seq[Instruction])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[Text] =

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
      .substring(leadingCy.length,insGroup.group.head.text.langs(1).length)
      .trim

    val firstBulletPointItems: Text = TextBuilder.fromPhrase(Phrase(firstBpEn, firstBpCy))

    // Process remaining bullet points
    val remainder: Seq[Text] = createRemainingItems(leadingEn, leadingCy, insGroup.group.drop(1))

    BulletPointList(TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)),
                    firstBulletPointItems +: remainder)
  }

}
