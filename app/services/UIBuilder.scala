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
import models.ocelot.stanzas.{Question => OcelotQuestion, Input => OcelotInput, CurrencyInput => OcelotCurrencyInput, _}
import models.ocelot.{Phrase, Link => OcelotLink, headingCallout}
import models.ui._
import play.api.Logger
import models.ocelot.isLinkOnlyPhrase

import scala.annotation.tailrec

@Singleton
class UIBuilder {
  val logger: Logger = Logger(getClass)

  def buildPage(url: String, stanzas: Seq[VisualStanza], formData: Option[FormData] = None)(implicit stanzaIdToUrlMap: Map[String, String]): Page =
    Page(url, fromStanzas(stackStanzas(stanzas, Nil), false, formData))

  def fromStanzas(stanzas: Seq[VisualStanza], useReducedHeadings: Boolean, formData: Option[FormData] = None)(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    stanzas match {
      case Nil => Nil
      case _ =>
        stanzas.foldLeft(Seq[UIComponent]()) {(acc, stanza) =>
          stanza match {
           case sg: StackedGroup => acc ++ fromStackedGroup(sg, formData)
           case i: Instruction => acc :+ fromInstruction(i)
           case ig: InstructionGroup => acc :+ fromInstructionGroup(ig)
           case c: Callout => acc ++ fromCallout(c, formData, useReducedHeadings)
           //case rg: RowGroup => acc :+ fromRowGroup(rg)
           case in: OcelotInput => Seq(fromInput(in, formData, acc))
           case q: OcelotQuestion => Seq(fromQuestion(q, acc))
           case _ => acc
          }
        }
    }

  @tailrec
  private def stackStanzas(stanzas: Seq[VisualStanza], acc: Seq[Seq[VisualStanza]]): Seq[VisualStanza] =
    stanzas match {
      case Nil => acc.collect{
        case s if s.length > 1 => StackedGroup(s)
        case s => s.head
      }
      case x :: xs if acc.isEmpty => stackStanzas(xs, Seq(Seq(x)))
      case x :: xs if x.stack => stackStanzas(xs, acc.init :+ (acc.last :+ x))
      case x :: xs => stackStanzas(xs, acc :+ Seq(x))
    }

  private def summaryList(rg: RowGroup): Boolean =
    rg.group.map(_.cells.length).max == 3 && rg.group.forall(r => r.cells.length < 3 || isLinkOnlyPhrase(r.cells(2)))

  private def fromStackedGroup(sg: StackedGroup, formData: Option[FormData])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    sg.group match {
      case (c: Callout) :: (rg: RowGroup) :: xs if headingCallout(c) && summaryList(rg) => // Summary List
        logger.info(s"SUMMARY LIST PATTERN")
        (fromCallout(c, formData, true) :+
         SummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))) ++
         fromStanzas(xs, true, formData)
      // case cp: Seq[Callout] if cp.forall(c => c.noteType == YourDecision) => // Confirmation callout with multiple lines
      case (c: Callout) :: (rg: RowGroup) :: xs if headingCallout(c) => // Table
        logger.info(s"TABLE PATTERN")
        fromStanzas(sg.group, sg.containsHeading, formData)
      case _ => // No recognised stacked pattern
        logger.info(s"NO PATTERN")
        sg.group.foreach(vs => println(s"VS: $vs"))
        fromStanzas(sg.group, sg.containsHeading, formData)
    }

  // private def fromRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] = {
  //   // val rowLength = rg.group.map(_.cells.length).max
  //   // SummaryList(rg.group.map{row =>
  //   //   (row.cells ++ Seq.fill(rowLength - row.cells.size)(Phrase())).map(phrase => TextBuilder.fromPhrase(phrase))
  //   // })
  //   Seq.empty
  // }

  private def fromInstruction( i:Instruction)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    i match {
      case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _, _) if OcelotLink.isLinkableStanzaId(dest) =>
        Paragraph(Text.link(stanzaIdToUrlMap(dest), txt.langs, window))
      case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _, _) => Paragraph(Text.link(dest, txt.langs, window))
      case Instruction(txt, _, _, _, _) => Paragraph(TextBuilder.fromPhrase(txt))
    }

  private def fromQuestion(q: OcelotQuestion, components: Seq[UIComponent]): UIComponent = {

    val answers = q.answers.map { ans =>
      val (answer, hint) = TextBuilder.singleTextWithOptionalHint(ans)
      Answer(answer, hint)
    }

    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (question, hint) = TextBuilder.singleTextWithOptionalHint(q.text)
    Question(question, hint, uiElements, answers, errorMsgs)
  }

  private def fromCallout(c: Callout, formData: Option[FormData], useReducedHeadings: Boolean)
                         (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] = {
    c.noteType match {
      case Title if useReducedHeadings => Seq(H1small(TextBuilder.fromPhrase(c.text)))
      case SubTitle if useReducedHeadings => Seq(H2small(TextBuilder.fromPhrase(c.text)))
      case Section if useReducedHeadings => Seq(H3small(TextBuilder.fromPhrase(c.text)))
      case SubSection if useReducedHeadings => Seq(H4small(TextBuilder.fromPhrase(c.text)))
      case Title => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case SubTitle => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case Section => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case SubSection => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case Lede => Seq(Paragraph(TextBuilder.fromPhrase(c.text), true))
      case Important => Seq(ErrorMsg("ID", TextBuilder.fromPhrase(c.text)))
      case Error =>
        // Ignore error messages if no errors exist within form data
        // TODO this should allocate the messages to errors found within the formData
        // as this linking of messages to form ids has not been resolved, Currently
        // this code will allocate all ErrorMsg elements to the only current error
        // which is error.required
        formData.fold[Seq[UIComponent]](Seq.empty)(data => data.errors.map(err => ErrorMsg(err.key, TextBuilder.fromPhrase(c.text))))
    }
  }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    def createBulletPointItems(leadingEn: String, leadingCy: String, remainder: Seq[Instruction])
                              (implicit stanzaIdToUrlMap: Map[String, String]): Seq[Text] =
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

    val leadingEn: String = BulletPointBuilder.determineMatchedLeadingText(insGroup, 0)
    val leadingCy: String = BulletPointBuilder.determineMatchedLeadingText(insGroup, 1)
    // Process bullet points
    val bulletPointListItems: Seq[Text] = createBulletPointItems(leadingEn, leadingCy, insGroup.group)

    BulletPointList(TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)), bulletPointListItems)
  }

  private def fromInput(input: OcelotInput, formData: Option[FormData], components: Seq[UIComponent])
                       (implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    // Placeholder not used
    input match {
      case i: OcelotCurrencyInput => CurrencyInput(name, hint, uiElements, errorMsgs)
    }
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

}
