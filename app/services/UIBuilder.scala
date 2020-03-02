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

import models.ocelot.stanzas.{Instruction,InstructionGroup,ValueStanza,EndStanza,Callout,Title,SubTitle,Lede,Error,Section}
import models.ocelot.Link
import models.ocelot.Phrase
import models.ui._

object UIBuilder {

  private def fromCallout(c: Callout): UIComponent =
    c.noteType match {
      case Title => H1(Text(c.text.langs))
      case SubTitle => H2(Text(c.text.langs))
      case Section => H3(Text(c.text.langs))
      case Lede => Paragraph(Seq(Text(c.text.langs)), true)
      case Error => H3(Text(c.text.langs)) // TODO
  }

  def fromInstructionGroup( instructionGroup: InstructionGroup, langIndex: Int  = 0 )(implicit stanzaIdToUrlMap: Map[String, String]) : BulletPointList = {

    val leadingTextEnglish: String = TextBuilder.determineMatchedLeadingText(
      instructionGroup.group.head.text.langs(0),
      instructionGroup.group(1).text.langs(0)
    )

    val leadingTextWelsh: String = TextBuilder.determineMatchedLeadingText(
      instructionGroup.group.head.text.langs(1),
      instructionGroup.group(1).text.langs(1)
    )

    val leadingPhrase: Phrase = Phrase( Vector( leadingTextEnglish, leadingTextWelsh ) )

    val leadingItems: Seq[TextItem] = TextBuilder.fromPhrase( leadingPhrase )

    // Determine first bullet point
    val firstBulletPointEnglish: String = instructionGroup.group.head.text.langs(0).substring(
      leadingTextEnglish.size,
      instructionGroup.group.head.text.langs(0).size ).trim

    val firstBulletPointWelsh: String = instructionGroup.group.head.text.langs(1).substring(
      leadingTextWelsh.size,
      instructionGroup.group.head.text.langs(1).size
    ).trim

    val firstBulletPointPhrase: Phrase = Phrase( Vector( firstBulletPointEnglish, firstBulletPointWelsh ) )

    val firstBulletPointItems: Seq[TextItem] = TextBuilder.fromPhrase( firstBulletPointPhrase )

    // Process remaining bullet points
    val remainder: Seq[Seq[TextItem]] = createRemainingBulletPointItems( leadingTextEnglish, leadingTextWelsh, instructionGroup.group.drop(1) )

    val bulletPointItems: Seq[Seq[TextItem]] = firstBulletPointItems +: remainder

    BulletPointList( leadingItems, bulletPointItems )
  }

  def createRemainingBulletPointItems( leadingTextEnglish: String,
                                       leadingTextWelsh: String,
                                       remainder: Seq[Instruction] )(implicit stanzaIdToUrlMap: Map[String, String]) : Seq[Seq[TextItem]] = {


    remainder.map {

      instruction => {

        val bulletPointEnglish: String = instruction.text.langs(0).substring(
          leadingTextEnglish.size,
          instruction.text.langs(0).size
        ).trim

        val bulletPointWelsh: String = instruction.text.langs(1).substring(
          leadingTextWelsh.size,
          instruction.text.langs(1).size
        ).trim

        val bulletPointPhrase: Phrase = Phrase( Vector( bulletPointEnglish, bulletPointWelsh ) )

        TextBuilder.fromPhrase( bulletPointPhrase )
      }
    }
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

          case ig: InstructionGroup => acc ++ Seq( fromInstructionGroup( ig ) )

          case models.ocelot.stanzas.Question(txt,ans,next,stack) =>
            val answers = (ans zip next).map{ t =>
              val (phrase, stanzaId) = t
              val (answer, hint) = TextBuilder.answerTextWithOptionalHint(phrase)
              Answer(answer, hint, stanzaIdToUrlMap(stanzaId))
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

