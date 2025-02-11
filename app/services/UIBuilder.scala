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

import javax.inject.Singleton
import models._
import models.ocelot.stanzas._
import core.models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Input, Question}
import core.models.ocelot.stanzas.{ExclusiveSequence, NonExclusiveSequence, _}
import core.models.ocelot.{Labels, Link, Phrase, EmbeddedParameterRegex, exclusiveOptionRegex}
import models.ui.{Answer, BulletPointList, ComplexDetails, ConfirmationPanel, CyaSummaryList, Details, ErrorMsg, H1, H2, H3, H4, InsetText, WarningText}
import models.ui.{NameValueSummaryList, Page, Paragraph, RequiredErrorMsg, Table, Text, TypeErrorMsg, UIComponent, ValueErrorMsg, stackStanzas}
import BulletPointBuilder.{Break, ExplicitBreak}
import play.api.Logger
import play.api.i18n.Lang
import scala.annotation.tailrec

case class UIContext(labels: Labels, lang: Lang, stanzaIdToUrlMap: Map[String, String])

sealed trait ErrorStrategy {
  def default(stanzas: Seq[VisualStanza]): ErrorStrategy = this
}
case object NoError extends ErrorStrategy
case object ValueMissingError extends ErrorStrategy
case class ValueMissingGroupError(missingFieldNames: List[String]) extends ErrorStrategy
case object ValueTypeError extends ErrorStrategy {
  override def default(stanzas: Seq[VisualStanza]): ErrorStrategy =
    stanzas.collect{case s:TypeErrorCallout => s}
           .headOption
           .fold[ErrorStrategy](ValueMissingError)(_ => this)
}

@Singleton
class UIBuilder {
  val logger: Logger = Logger(getClass)

  def buildPage(url: String, stanzas: Seq[VisualStanza], errStrategy: ErrorStrategy = NoError)(implicit ctx: UIContext): Page = {
    val stanzaTransformPipeline: List[Seq[VisualStanza] => Seq[VisualStanza]] =
      List(expandLabelReferences(Nil), BulletPointBuilder.groupBulletPointInstructions(Nil), Aggregator.aggregateStanzas(Nil), stackStanzas(Nil))
    Page(url, fromStanzas(stanzaTransformPipeline.foldLeft(stanzas){case (s, t) => t(s)}, Nil, errStrategy.default(stanzas)))
  }

  private def expandLabelReferences(acc: List[VisualStanza])(stanzas: Seq[VisualStanza])(implicit ctx: UIContext): Seq[VisualStanza] =
    stanzas match {
      case Nil => acc.reverse
      case (i: Instruction) +: xs => expandLabelReferences(i.copy(text = TextBuilder.expandLabels(i.text, ctx.labels)) :: acc)(xs)
      case (n: NoteCallout) +: xs => expandLabelReferences(n.copy(text = TextBuilder.expandLabels(n.text, ctx.labels)) :: acc)(xs)
      case s +: xs => expandLabelReferences(s :: acc)(xs)
    }

  @tailrec
  private def fromStanzas(stanzas: Seq[VisualStanza], acc: Seq[UIComponent], errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    stanzas match {
      case Nil => acc
      case (sg: StackedGroup) :: xs => fromStanzas(xs, acc ++ fromStackedGroup(sg, errStrategy), errStrategy)
      case (eg: RequiredErrorGroup) :: xs => fromStanzas(xs, acc ++ fromRequiredErrorGroup(eg, errStrategy), errStrategy)
      case (i: Instruction) :: xs => fromStanzas(xs, acc ++ Seq(fromInstruction(i)), errStrategy)
      case (ig: InstructionGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromInstructionGroup(ig)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isCYASummaryList => fromStanzas(xs, acc ++ Seq(fromCYASummaryListRowGroup(rg)), errStrategy)
      case (rg: RowGroup) :: xs if rg.isNameValueSummaryList => fromStanzas(xs, acc ++ Seq(fromNameValueSummaryListRowGroup(rg)), errStrategy)
      case (nl: NumberedList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedList(nl)), errStrategy)
      case (nl: NumberedCircleList) :: xs => fromStanzas(xs, acc ++ Seq(fromNumberedCircleList(nl)), errStrategy)
      case (c: Callout) :: xs => fromStanzas(xs, acc ++ fromCallout(c, errStrategy), errStrategy)
      case (in: Input) :: xs => fromStanzas(Nil, Seq(fromInput(in, acc)), errStrategy)
      case (q: Question) :: xs => fromStanzas(Nil, Seq(fromQuestion(q, acc)), errStrategy)
      case (ns: NonExclusiveSequence) :: xs => fromStanzas(Nil, Seq(fromNonExclusiveSequence(ns, acc)), errStrategy)
      case (es: ExclusiveSequence) :: xs => fromStanzas(Nil, Seq(fromExclusiveSequence(es, acc)), errStrategy)
      case (ng: NoteGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromNoteGroup(ng)), errStrategy)
      case (wt: ImportantGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromImportantGroup(wt)), errStrategy)
      case (ycg: YourCallGroup) :: xs => fromStanzas(xs, acc ++ Seq(fromYourCallGroup(ycg)), errStrategy)
      case x :: xs =>
        logger.error(s"Encountered and ignored VisualStanza invalid due to accessibility rules, $x")
        fromStanzas(xs, acc, errStrategy)
    }

  private def fromStackedGroup(sg: StackedGroup, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    sg.group match {
      case (c: SubSectionCallout) :: (rg: RowGroup) :: xs if rg.isTableCandidate =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromTableRowGroup(TextBuilder.fromPhrase(c.text), rg)), errStrategy)
      case (c: SubSectionCallout) :: (ng: NoteGroup) :: xs  =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromSectionAndNoteGroup(TextBuilder.fromPhrase(c.text), ng)), errStrategy)
      case (c: SubSectionCallout) :: (nc: NoteCallout) :: xs  =>
        fromStanzas(stackStanzas(Nil)(xs), Seq(fromSectionAndNoteCallout(TextBuilder.fromPhrase(c.text), nc)), errStrategy)
      case x :: xs => // No recognised stacked pattern
        fromStanzas(x +: stackStanzas(Nil)(xs), Nil, errStrategy)
    }

  private def fromCYASummaryListRowGroup(rg: RowGroup)(implicit ctx: UIContext): UIComponent =
    CyaSummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromNameValueSummaryListRowGroup(rg: RowGroup)(implicit ctx: UIContext): UIComponent =
    NameValueSummaryList(rg.paddedRows.map(row => row.map(phrase => TextBuilder.fromPhrase(phrase))))

  private def fromTableRowGroup(caption: Text, rg: RowGroup)(implicit ctx: UIContext): UIComponent = {
    val tableRows: Seq[Seq[Text]] = rg.paddedRows.map(r => r.map(phrase => TextBuilder.fromPhrase(phrase)))
    Table(caption, tableRows.head, tableRows.tail)
  }

  private def fromNumberedList(nl: NumberedList)(implicit ctx: UIContext): UIComponent =
    ui.NumberedList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromNumberedCircleList(nl: NumberedCircleList)(implicit ctx: UIContext): UIComponent =
    ui.NumberedCircleList(nl.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromInstruction( i:Instruction)(implicit ctx: UIContext): UIComponent =
    i match {
      case Instruction(txt, _, Some(Link(id, dest, _, window)), _, _) if Link.isLinkableStanzaId(dest) =>
        Paragraph(Text.link(ctx.stanzaIdToUrlMap(dest), txt.value(ctx.lang), window))
      case Instruction(txt, _, Some(Link(id, dest, _, window)), _, _) => Paragraph(Text.link(dest, txt.value(ctx.lang), window))
      case Instruction(txt, _, _, _, _) => Paragraph(TextBuilder.fromPhrase(txt))
    }

  private def fromQuestion(q: Question, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    val answers = q.answers.map { ans =>
      val (answer, hint) = TextBuilder.fromPhraseWithOptionalHint(ans)
      Answer(answer, hint)
    }
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (question, hint) = TextBuilder.fromPhraseWithOptionalHint(q.text)
    ui.Question(question, hint, uiElements, answers, errorMsgs)
  }

  private def fromCallout(co: Callout, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    co match {
      case c: TitleCallout => Seq(H1(TextBuilder.fromPhrase(c.text)))
      case c: SubTitleCallout => Seq(H2(TextBuilder.fromPhrase(c.text)))
      case c: SectionCallout => Seq(H3(TextBuilder.fromPhrase(c.text)))
      case c: SubSectionCallout => Seq(H4(TextBuilder.fromPhrase(c.text)))
      case c: LedeCallout => Seq(Paragraph(TextBuilder.fromPhrase(c.text), lede = true))
      case c: YourCallCallout => Seq(ConfirmationPanel(TextBuilder.fromPhrase(c.text)))
      case c: NoteCallout => Seq(InsetText(Seq(TextBuilder.fromPhrase(c.text))))
      case c: TypeErrorCallout if errStrategy == ValueTypeError => Seq(TypeErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ErrorCallout if errStrategy == ValueMissingError => Seq(RequiredErrorMsg(TextBuilder.fromPhrase(c.text)))
      case c: ValueErrorCallout => Seq(ValueErrorMsg(TextBuilder.fromPhrase(c.text)))
      case _: TypeErrorCallout | _: ErrorCallout => Seq.empty   // Consume unmatched Error callouts
      case c: ImportantCallout =>  Seq(WarningText(Seq(TextBuilder.fromPhrase(c.text))))
      case _: NumberedListItemCallout => Seq.empty              // Unused
      case _: NumberedCircleListItemCallout => Seq.empty        // Unused
    }

  private def fromInstructionGroup(insGroup: InstructionGroup)(implicit ctx: UIContext): UIComponent = {

    val phraseGroup: Seq[Phrase] = insGroup.group.map(_.text)

    val bulletPointComponents: Seq[Text] = createBulletPointListComponents(phraseGroup)

    BulletPointList(bulletPointComponents.head, bulletPointComponents.tail)
  }

  private def fromInput(input: Input, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    // Split out an Error callouts from body components
    val (errorMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val name = TextBuilder.fromPhrase(input.name)
    val hint = input.help.map(phrase => TextBuilder.fromPhrase(phrase))
    input match {
      case _: TextInput => ui.TextInput(name, hint, uiElements, errorMsgs)
      case _: NumberInput => ui.NumberInput(name, hint, uiElements, errorMsgs)
      case _: CurrencyInput => ui.CurrencyInput(name, hint, uiElements, errorMsgs)
      case _: CurrencyPoundsOnlyInput => ui.CurrencyPoundsOnlyInput(name, hint, uiElements, errorMsgs)
      case _: DateInput => ui.DateInput(name, hint, uiElements, errorMsgs)
    }
  }

  @tailrec
  private def partitionComponents(components: Seq[UIComponent], errors: Seq[ErrorMsg], others: Seq[UIComponent]): (Seq[ErrorMsg], Seq[UIComponent]) =
    components match {
      case Nil => (errors.reverse, others.reverse)
      case (e: ErrorMsg) :: xs => partitionComponents(xs, e +: errors, others)
      case x :: xs => partitionComponents(xs, errors, x +: others)
    }

  private def fromRequiredErrorGroup(eg: RequiredErrorGroup, errStrategy: ErrorStrategy)(implicit ctx: UIContext): Seq[UIComponent] =
    errStrategy match {
      case ValueMissingGroupError(Nil) =>
        eg.group.find(co => EmbeddedParameterRegex.findAllMatchIn(co.text.value(ctx.lang)).length == 0).fold[Seq[UIComponent]](Nil){errorCallout =>
          Seq(RequiredErrorMsg(Text(errorCallout.text.value(ctx.lang))))
        }
      case e: ValueMissingGroupError =>
        eg.group.find(co => EmbeddedParameterRegex.findAllMatchIn(co.text.value(ctx.lang)).length == e.missingFieldNames.length)
                .fold[Seq[UIComponent]](Nil){errorCallout =>
                  Seq(RequiredErrorMsg(Text(EmbeddedParameterRegex.replaceSomeIn(errorCallout.text.value(ctx.lang), { m =>
                    Option(m.group(1)).map(_.toInt).fold[Option[String]](None)(idx => e.missingFieldNames.lift(idx))
                  }))))
                }
      case _ => Nil
    }

  private def fromNoteGroup(ng: NoteGroup)(implicit ctx: UIContext): UIComponent =
    InsetText(ng.group.map(co => TextBuilder.fromPhrase(co.text)))

  private def fromImportantGroup(wt: ImportantGroup)(implicit ctx: UIContext): UIComponent =
    WarningText(wt.group.map(wc => TextBuilder.fromPhrase(wc.text)))

  private def fromYourCallGroup(ycg: YourCallGroup)(implicit ctx: UIContext): UIComponent = {
    val texts: Seq[Text] = ycg.group.map(c => TextBuilder.fromPhrase(c.text))
    ConfirmationPanel(texts.head, texts.tail)
  }

  private def fromSectionAndNoteGroup(caption: Text, ng: NoteGroup)(implicit ctx: UIContext): UIComponent = {

    val noteCallouts: Seq[Seq[Phrase]] = BulletPointBuilder.groupBulletPointNoteCalloutPhrases(Nil)(ng.group)

    val detailsComponents: Seq[Seq[Text]] = noteCallouts.map{ phraseSeq =>
      if(phraseSeq.size > 1) createBulletPointListComponents(phraseSeq) else Seq(TextBuilder.fromPhrase(phraseSeq.head))
    }

    if(detailsComponents.forall(_.size == 1)) {
      Details(caption, detailsComponents.flatten)
    } else {
      ComplexDetails(caption, detailsComponents)
    }
  }

  private def fromSectionAndNoteCallout(caption: Text, nc: NoteCallout)(implicit ctx: UIContext): UIComponent =
    Details(caption, Seq(TextBuilder.fromPhrase(nc.text)))

  private def fromNonExclusiveSequence(nonExclusiveSequence: NonExclusiveSequence, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {
    val (errMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)

    val (text, hint) = TextBuilder.fromPhraseWithOptionalHint(nonExclusiveSequence.text)
    val options: Seq[Text] = nonExclusiveSequence.options.map{phrase => TextBuilder.fromPhrase(phrase)}

    ui.NonExclusiveSequence(text, hint, options, uiElements, errMsgs)
  }

  private def fromExclusiveSequence(exclusiveSequence: ExclusiveSequence, components: Seq[UIComponent])(implicit ctx: UIContext): UIComponent = {

    val (errMsgs, uiElements) = partitionComponents(components, Seq.empty, Seq.empty)
    val (text, hint) = TextBuilder.fromPhraseWithOptionalHint(exclusiveSequence.text)
    val options: Seq[Text] = exclusiveSequence.nonExclusiveOptions.map{phrase => TextBuilder.fromPhrase(phrase)}

    val exclusiveOptionPhrase: Phrase = Phrase(
      exclusiveOptionRegex.replaceAllIn(exclusiveSequence.exclusiveOptions.head.english,"").trim,
      exclusiveOptionRegex.replaceAllIn(exclusiveSequence.exclusiveOptions.head.welsh,"").trim
    )

    ui.ExclusiveSequence(text, hint, options, TextBuilder.fromPhrase(exclusiveOptionPhrase), uiElements, errMsgs)
  }

  private def createBulletPointListComponents(phraseGroup: Seq[Phrase])(implicit ctx: UIContext): Seq[Text] =
    if(phraseGroup.head.english.contains(ExplicitBreak)) {
      createBulletPointListComponentsFromExplicitlyMatchedGroup(phraseGroup)
    } else {
      createBulletPointListComponentsFromImplicitlyMatchedGroup(phraseGroup)
    }


  def createBulletPointListComponentsFromExplicitlyMatchedGroup(phraseGroup: Seq[Phrase])(implicit ctx: UIContext): Seq[Text] = {

    val leadingEn: String = phraseGroup.head.english.substring(0, phraseGroup.head.english.indexOf(ExplicitBreak))
    val leadingCy: String = phraseGroup.head.welsh.substring(0, phraseGroup.head.welsh.indexOf(ExplicitBreak))

    val cleansedPhraseGroup: Seq[Phrase] = phraseGroup.map(p => Phrase(p.english.replaceFirst("\\[" + Break + "\\]", ""),
      p.welsh.replaceFirst("\\[" + Break + "\\]", "")))

    val bulletPointListItems: Seq[Text] = createBulletPointItems(leadingEn.length, leadingCy.length, cleansedPhraseGroup)

    TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)) +: bulletPointListItems
  }

  def createBulletPointListComponentsFromImplicitlyMatchedGroup(phraseGroup: Seq[Phrase])(implicit ctx: UIContext): Seq[Text] = {

    val leadingEn: String = BulletPointBuilder.determineMatchedLeadingText(phraseGroup, _.english)
    val leadingCy: String = BulletPointBuilder.determineMatchedLeadingText(phraseGroup, _.welsh)
    // Process bullet points
    val bulletPointListItems: Seq[Text] = createBulletPointItems(leadingEn.length, leadingCy.length, phraseGroup)

    TextBuilder.fromPhrase(Phrase(leadingEn, leadingCy)) +: bulletPointListItems
  }

  private def createBulletPointItems(leadingEnLength: Int, leadingCyLength: Int, items: Seq[Phrase])(implicit ctx: UIContext): Seq[Text] =
    items.map{phrase =>
      val bulletPointEnglish: String = phrase.english.substring(leadingEnLength, phrase.english.length).trim
      val bulletPointWelsh: String = phrase.welsh.substring(leadingCyLength, phrase.welsh.length).trim

      TextBuilder.fromPhrase(Phrase(bulletPointEnglish, bulletPointWelsh))
    }
}
