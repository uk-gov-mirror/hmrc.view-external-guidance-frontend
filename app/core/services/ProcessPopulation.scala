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

package core.services

import core.models.ocelot.stanzas._
import core.models.ocelot.{Link, Phrase, Process, pageLinkIds}
import core.models.ocelot.errors._
import play.api.Logger
import scala.annotation.tailrec

trait ProcessPopulation {
  this: PlaceholderProvider =>

  val logger: Logger

  def stanza(id: String, process: Process): Either[GuidanceError, Stanza] =
    process.flow.get(id) match {
      case Some(stanza) => populateStanza(id, stanza, process)
      case None => Left(StanzaNotFound(id))
    }

  private def populateStanza(id: String, stanza: Stanza, process: Process): Either[GuidanceError, Stanza] = {

    def populateInstruction(i: InstructionStanza): Either[GuidanceError, Instruction] =
      phrase(i.text, id, process).fold(
        Left(_),
        text => {
          i.link match {
            case Some(linkIndex) => link(linkIndex).fold(Left(_), link => Right(Instruction(i, text, Some(link), pageLinkIds(text.english))))
            case None => Right(Instruction(i, text, None, pageLinkIds(text.english)))
          }
        }
      )

    def populateQuestion(q: QuestionStanza): Either[GuidanceError, Question] =
      phrases(q.text +: q.answers, Nil, id, process) match {
        case Right(texts) if q.answers.length != q.next.length => Left(InconsistentQuestionError(id))
        case Right(texts) => Right(Question(q, texts.head, texts.tail))
        case Left(err) => Left(err)
      }

    def populateInput(i: InputStanza): Either[GuidanceError, Input] =
      phrase(i.name, id, process).fold(Left(_), name =>
        optionalPhrase(i.help, id, process).fold(Left(_), help =>
          optionalPhrase(i.placeholder, id, process).fold(Left(_), placeholder => Right(Input(i, name, help, placeholder)))
        )
      )

    def populateRow(r: RowStanza): Either[GuidanceError, Row] =
      phrases(r.cells, Nil, id, process) match {
        case Right(texts) => Right(Row(r, texts, pageLinkIds(texts)))
        case Left(err) => Left(err)
      }

    def populateSequence(id: String, s: SequenceStanza): Either[GuidanceError, Sequence] =
      phrases(s.options, Nil, id, process).fold(Left(_), options =>
        phrase(s.text, id, process).fold(Left(_), text => Right(Sequence(s, text, options)))
      )

    def link(linkIndex: Int): Either[LinkNotFound, Link] =
      process.linkOption(linkIndex).map(Right(_)).getOrElse(Left(LinkNotFound(id, linkIndex)))

    stanza match {
      case q: QuestionStanza => populateQuestion(q)
      case r: RowStanza => populateRow(r)
      case i: InstructionStanza => populateInstruction(i)
      case i: InputStanza => populateInput(i)
      case c: CalloutStanza => phrase(c.text, id, process).fold(Left(_), text => Right(Callout(c, text)))
      case c: ChoiceStanza =>
        Right(Choice(c.copy(tests = c.tests.map(t => t.copy(left = placeholders.translate(t.left), right = placeholders.translate(t.right))))))
      case c: CalculationStanza =>
        Right(Calculation(c.copy(calcs = c.calcs.map(op => op.copy(left = placeholders.translate(op.left), right = placeholders.translate(op.right))))))
      case s: SequenceStanza => populateSequence(id, s)
      case vs: ValueStanza => Right(vs.copy(values = vs.values.map(v => v.copy(value = placeholders.translate(v.value)))))
      case s: Stanza => Right(s)
    }
  }

  private def optionalPhrase(index: Option[Int], stanzaId: String, process: Process):Either[GuidanceError, Option[Phrase]] =
    index.fold[Either[GuidanceError, Option[Phrase]]](Right(None))(i => phrase(i, stanzaId, process).fold(Left(_), phrase => Right(Some(phrase))))

  private def phrase(phraseIndex: Int, stanzaId: String, process: Process): Either[GuidanceError, Phrase] =
    process.phraseOption(phraseIndex).fold[Either[GuidanceError, Phrase]](Left(PhraseNotFound(stanzaId, phraseIndex))){
      case Phrase(english, welsh) if welsh.trim.isEmpty && english.trim.nonEmpty => Left(MissingWelshText(stanzaId, phraseIndex.toString, english))
      case Phrase(english, welsh) if welsh.trim.startsWith("Welsh,") =>
        logger.warn(s"Found obsolete faked Welsh prefix on phrase $english -- $welsh")
        val updatedWelsh = s"Welsh: ${welsh.trim.drop("Welsh, ".length)}"
        Right(Phrase(placeholders.translate(english), placeholders.translate(updatedWelsh)))
      case p: Phrase =>
        Right(Phrase(placeholders.translate(p.english), placeholders.translate(p.welsh)))
    }

  @tailrec
  private def phrases(indexes: Seq[Int], acc: Seq[Phrase], stanzaId: String, process: Process): Either[GuidanceError, Seq[Phrase]] =
    indexes match {
      case Nil => Right(acc)
      case index +: xs =>
        phrase(index, stanzaId, process) match {
          case Right(phrase) => phrases(xs, acc :+ phrase, stanzaId, process)
          case Left(err) => Left(err)
        }
    }
}
