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
import models.ocelot.stanzas.{Question => OQuestion, Input => OInput, CurrencyInput => OCurrencyInput, _}
import models.ocelot.{Phrase, Link => OcelotLink}
import models.ui.{NumberedList, NumberedCircleList, _}
import play.api.Logger

import scala.annotation.tailrec

@Singleton
class UIBuilder {
  val logger: Logger = Logger(getClass)
  val stanzaTransformPipeline: Seq[Seq[VisualStanza] => Seq[VisualStanza]] =
    Seq(BulletPointBuilder.groupBulletPointInstructions(Nil),
        Aggregator.aggregateStanzas(Nil),
        stackStanzas(Nil))

  def buildPage(url: String, stanzas: Seq[VisualStanza], formData: Option[FormData] = None)
               (implicit stanzaIdToUrlMap: Map[String, String]): Page = {
    val groupedStanzas: Seq[VisualStanza] = stanzaTransformPipeline.foldLeft(stanzas){case (s, t) => t(s)}
    Page(url, fromStanzas(groupedStanzas, Nil, formData))
  }

  @tailrec
  private def fromStanzas(stanzas: Seq[VisualStanza], acc: Seq[UIComponent], formData: Option[FormData])
                 (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    stanzas match {
      case Nil => acc
      case (sg: StackedGroup) :: xs => fromStanzas(xs, acc ++ fromStackedGroup(sg, formData), formData)
      case (i: Instruction) :: xs => fromStanzas(xs, acc ++ Seq(fromInstruction(i)), formData)
      case (ig: InstructionGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromInstructionGroup(ig)), formData)
      case (rg: RowGroup) :: xs if rg.isSummaryList => fromStanzas(xs, acc ++ Seq(fromSummaryListRowGroup(rg)), formData)
      case (rg: RowGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromTableRowGroup(None, rg)), formData)
      case (nl: NumListGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromNumListGroup(nl)), formData)
      case (nl: NumCircListGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromNumCircListGroup(nl)), formData)
      case (c: Callout) :: xs => fromStanzas(xs, acc ++ fromCallout(c, formData), formData)
      case (in: OInput) :: xs => fromStanzas(Nil, Seq(fromInput(in, acc)), formData)
      case (q: OQuestion) :: xs => fromStanzas(Nil, Seq(fromQuestion(q, acc)), formData)
      case x :: xs => fromStanzas(xs, acc, formData)
    }

  private def fromStackedGroup(sg: StackedGroup, formData: Option[FormData])
                              (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] = {
    sg.group match {
      case (c: SubSectionCallout) :: (rg: RowGroup) :: xs if !rg.isSummaryList =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromTableRowGroup(Some(TextBuilder.fromPhrase(c.text)), rg)), formData)
      case (c1:YourCallCallout) :: (c2:YourCallCallout) :: xs =>
        fromSequenceWithLeadingYourCallCallouts(sg, formData)
      case x :: xs => // No recognised stacked pattern
        fromStanzas( x +: stackStanzas(Nil)(xs), Nil, formData)
    }
  }

  private def fromSummaryListRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    SummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromTableRowGroup(caption: Option[Text], rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    val rows: Seq[Seq[Cell]] = rg.paddedRows.map{rw =>
      rw.map{phrase =>
        TextBuilder.fromPhrase(phrase) match {
          case x if x.isBold => Th(x)
          case x => Td(x)
        }
      }
    }
    rows.headOption.fold(Table(caption, None, Seq.empty)){row0 =>
      val heading = if (row0.collect{case c: Th => c}.length == row0.length) Some(row0) else None
      val tableRows = heading.fold(rows)(_ => rows.tail)
      Table(caption, heading, tableRows)
    }
  }

  private def fromNumListGroup(nl: NumListGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    NumberedList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromNumCircListGroup(nl: NumCircListGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    NumberedCircleList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromInstruction( i:Instruction)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    i match {
      case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _, _) if OcelotLink.isLinkableStanzaId(dest) =>
        Paragraph(Text.link(stanzaIdToUrlMap(dest), txt.langs, window))
      case Instruction(txt, _, Some(OcelotLink(id, dest, _, window)), _, _) => Paragraph(Text.link(dest, txt.langs, window))
      case Instruction(txt, _, _, _, _) => Paragraph(TextBuilder.fromPhrase(txt))
    }

  private def fromQuestion(q: OQuestion, components: Seq[UIComponent]): UIComponent = {
    val answers = q.answers.map { ans =>
      val (answer, hint) = TextBuilder.singleTextWithOptionalHint(ans)
      Answer(answer, hint)
    }

    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (question, hint) = TextBuilder.singleTextWithOptionalHint(q.text)
    Question(question, hint, uiElements, answers, errorMsgs)
  }

  private def fromCallout(co: Callout, formData: Option[FormData])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    co match {
      case c: TitleCallout => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case c: SubTitleCallout => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case c: SectionCallout => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case c: SubSectionCallout => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case c: LedeCallout => Seq(Paragraph(TextBuilder.fromPhrase(c.text), true))
      case c: ImportantCallout => Seq.empty // Reserved for future use
      case c: TypeErrorCallout => Seq(ErrorMsg("Type.ID", TextBuilder.fromPhrase(c.text)))
      case c: ValueErrorCallout => Seq(ErrorMsg("Value.ID", TextBuilder.fromPhrase(c.text)))
      case c: YourCallCallout => Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
      case c: NumListCallout => Seq(NumberedList(Seq(TextBuilder.fromPhrase(c.text))))
      case c: NumCircListCallout => Seq(NumberedCircleList(Seq(TextBuilder.fromPhrase(c.text))))
      case c: ErrorCallout =>
        // Ignore error messages if no errors exist within form data
        // TODO this should allocate the messages to errors found within the formData
        // as this linking of messages to form ids has not been resolved, Currently
        // this code will allocate all ErrorMsg elements to the only current error
        // which is error.required
        formData.fold[Seq[UIComponent]](Seq.empty)(data => data.errors.map(err => ErrorMsg(err.key, TextBuilder.fromPhrase(c.text))))
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

  private def fromInput(input: OInput, components: Seq[UIComponent])
                       (implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    // Placeholder not used
    input match {
      case i: OCurrencyInput => CurrencyInput(name, hint, uiElements, errorMsgs)
    }
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromSequenceWithLeadingYourCallCallouts(sg: StackedGroup, formData: Option[FormData])
                              (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] = {
    val (callouts, visualStanzas): (Seq[Callout], Seq[VisualStanza]) = sg.group.span{
      case c: YourCallCallout => true
      case _ => false
    } match {case (coStanzas, v) => (coStanzas.map{case c: YourCallCallout => c}, v)}

    fromStanzas(stackStanzas(Nil)(visualStanzas), Seq(fromYourCallGroup(callouts)), formData)
  }

  private def fromYourCallGroup(group: Seq[Callout])(implicit stanzaIdToUrlMap: Map[String, String]) : ConfirmationPanel = {

    val texts: Seq[Text] = group.map(c => TextBuilder.fromPhrase(c.text))

    ConfirmationPanel(texts.head, texts.tail)
  }

}
