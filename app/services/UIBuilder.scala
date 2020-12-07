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
import models.ocelot.stanzas.{CurrencyPoundsOnlyInput => OcelotCurrencyPOInput, Question => OcelotQuestion, Input => OcelotInput, CurrencyInput => OcelotCurrencyInput, _}
import models.ocelot.{Phrase, Link => OcelotLink}
import models.ocelot.stanzas.{NumberedList => OcelotNumberedList, NumberedCircleList => OcelotNumberedCircleList}
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
      case (rg: RowGroup) :: xs if rg.isCYASummaryList => fromStanzas(xs, acc ++ Seq(fromCYASummaryListRowGroup(rg)), formData)
      case (rg: RowGroup) :: xs if rg.isTable => fromStanzas(xs, acc ++ Seq(fromTableRowGroup(None, rg)), formData)
      case (rg: RowGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromSummaryListRowGroup(rg)), formData)
      case (nl: OcelotNumberedList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedList(nl)), formData)
      case (nl: OcelotNumberedCircleList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedCircleList(nl)), formData)
      case (c: Callout) :: xs => fromStanzas(xs, acc ++ fromCallout(c, formData), formData)
      case (in: OcelotInput) :: xs => fromStanzas(Nil, Seq(fromInput(in, acc)), formData)
      case (q: OcelotQuestion) :: xs => fromStanzas(Nil, Seq(fromQuestion(q, acc)), formData)
      case (ng: NoteGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromNoteGroup(ng)), formData)
      case (ycg: YourCallGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromYourCallGroup(ycg)), formData)
      case x :: xs =>
        logger.warn(s"Encountered unexpected VisualStanza $x")
        fromStanzas(xs, acc, formData)
    }

  private def fromStackedGroup(sg: StackedGroup, formData: Option[FormData])
                              (implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    sg.group match {
      case (c: SubSectionCallout) :: (rg: RowGroup) :: xs if rg.isTable =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromTableRowGroup(Some(TextBuilder.fromPhrase(c.text)), rg)), formData)
      case x :: xs => // No recognised stacked pattern
        fromStanzas(x +: stackStanzas(Nil)(xs), Nil, formData)
    }

  private def fromCYASummaryListRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    SummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromSummaryListRowGroup(rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    SummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromTableRowGroup(caption: Option[Text], rg: RowGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    def tableFromCaptionAndRows(captionText: Text, rows: Seq[Seq[Cell]]) =
      rows.headOption.fold(Table(captionText, None, Seq.empty)){row0 =>
        val heading: Option[Seq[Cell]] = if (row0.collect{case c: Th => c}.length == row0.length) Some(row0) else None
        val tableRows = heading.fold(rows)(_ => rows.tail)
        Table(captionText, heading, tableRows)
      }

    def tableFromRows(rows: Seq[Seq[Cell]]) =
      rows.headOption.fold(Table(Text(), None, Seq.empty)){row0 =>
        tableFromCaptionAndRows(row0.headOption.fold(Text())(_.text), rows.tail)
      }

    val rows: Seq[Seq[Cell]] = rg.paddedRows.map{rw =>
      rw.map{phrase =>
        TextBuilder.fromPhrase(phrase) match {
          case x if x.isBold => Th(x)
          case x => Td(x)
        }
      }
    }

    caption.fold(tableFromRows(rows))(c => tableFromCaptionAndRows(c, rows))
  }

  private def fromNumberedList(nl: OcelotNumberedList)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    NumberedList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromNumberedCircleList(nl: OcelotNumberedCircleList)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    NumberedCircleList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

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

  private def fromCallout(co: Callout, formData: Option[FormData])(implicit stanzaIdToUrlMap: Map[String, String]): Seq[UIComponent] =
    co match {
      case c: TitleCallout => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case c: SubTitleCallout => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case c: SectionCallout => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case c: SubSectionCallout => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case c: LedeCallout => Seq(Paragraph(TextBuilder.fromPhrase(c.text), lede = true))
      case c: TypeErrorCallout => Seq(ErrorMsg("Type.ID", TextBuilder.fromPhrase(c.text)))
      case c: ValueErrorCallout => Seq(ErrorMsg("Value.ID", TextBuilder.fromPhrase(c.text)))
      case c: YourCallCallout => Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
      case c: NoteCallout => Seq(InsetText(Seq(TextBuilder.fromPhrase(c.text))))
      case c: ErrorCallout =>
        // Ignore error messages if no errors exist within form data
        formData.fold[Seq[UIComponent]](Seq.empty)(data => data.errors.map(err => ErrorMsg(err.key, TextBuilder.fromPhrase(c.text))))
      case _: ImportantCallout => Seq.empty               // Reserved for future use
      case _: NumberedListItemCallout => Seq.empty        // Unused
      case _: NumberedCircleListItemCallout => Seq.empty  // Unused
    }

  // private def fromCallout(c: TitleCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(H1(TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: SubTitleCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(H2(TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: SectionCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(H3(TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: SubSectionCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(H4(TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: LedeCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(Paragraph(TextBuilder.fromPhrase(c.text), lede = true))
  // private def fromCallout(c: TypeErrorCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(ErrorMsg("Type.ID", TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: ValueErrorCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(ErrorMsg("Value.ID", TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: YourCallCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
  // private def fromCallout(c: NoteCallout, formData: Option[FormData]): Seq[UIComponent] = Seq(InsetText(Seq(TextBuilder.fromPhrase(c.text))))
  // private def fromCallout(c: ErrorCallout, formData: Option[FormData]): Seq[UIComponent] =
  //   formData.fold[Seq[UIComponent]](Seq.empty)(data => data.errors.map(err => ErrorMsg(err.key, TextBuilder.fromPhrase(c.text))))
  // private def fromCallout(c: ImportantCallout, formData: Option[FormData]): Seq[UIComponent] = Seq.empty

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

  private def fromInput(input: OcelotInput, components: Seq[UIComponent])
                       (implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    // Placeholder not used
    input match {
      case i: OcelotCurrencyInput => CurrencyInput(name, hint, uiElements, errorMsgs)
      case i: OcelotCurrencyPOInput => CurrencyPoundsOnlyInput(name, hint, uiElements, errorMsgs)
    }
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromNoteGroup(ng: NoteGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent =
    InsetText(ng.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromYourCallGroup(ycg: YourCallGroup)(implicit stanzaIdToUrlMap: Map[String, String]): UIComponent = {
    val texts: Seq[Text] = ycg.group.map(c => TextBuilder.fromPhrase(c.text))
    ConfirmationPanel(texts.head, texts.tail)
  }
}
