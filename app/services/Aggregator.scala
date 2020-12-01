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

import models.ocelot.stanzas._

import scala.annotation.tailrec

object Aggregator {

  @tailrec
  def aggregateStanzas(acc: Seq[VisualStanza])(inputSeq: Seq[VisualStanza]): Seq[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs =>
        val (rows: Seq[Row], remainder) = aggregateRows(xs, Seq (x))
        aggregateStanzas(acc :+ RowGroup (rows))(remainder)

      case (x: NumListCallout) :: xs =>
        val (cos: Seq[NumListCallout], remainder) = aggregateNumLists(xs, Seq (x))
        aggregateStanzas(acc :+ NumListGroup(cos))(remainder)

      case (x: NumCircListCallout) :: xs =>
        val (cos: Seq[NumCircListCallout], remainder) = aggregateNumCircLists(xs, Seq (x))
        aggregateStanzas(acc :+ NumCircListGroup(cos))(remainder)

      case (x: NoteCallout) :: xs =>
        val (cos: Seq[NoteCallout], remainder) = aggregateNotes(xs, Seq (x))
        aggregateStanzas(acc :+ NoteGroup(cos))(remainder)

      case (x: YourCallCallout) :: xs =>
        val (cos: Seq[YourCallCallout], remainder) = aggregateYourCall(xs, Seq (x))
        aggregateStanzas(acc :+ YourCallGroup(cos))(remainder)

      case x :: xs => aggregateStanzas(acc :+ x)(xs)
    }

  @tailrec
  private def aggregateRows(inputSeq: Seq[VisualStanza], acc: Seq[Row]): (Seq[Row], Seq[VisualStanza]) =
    inputSeq match {
      case (x: Row) :: xs if x.stack => aggregateRows(xs, acc :+ x)
      case xs => (acc, xs)
    }

  @tailrec
  private def aggregateNumLists(inputSeq: Seq[VisualStanza], acc: Seq[NumListCallout]): (Seq[NumListCallout], Seq[VisualStanza]) =
    inputSeq match {
      case (x: NumListCallout) :: xs if x.stack => aggregateNumLists(xs, acc :+ x)
      case xs => (acc, xs)
    }

  @tailrec
  private def aggregateNumCircLists(inputSeq: Seq[VisualStanza], acc: Seq[NumCircListCallout]): (Seq[NumCircListCallout], Seq[VisualStanza]) =
    inputSeq match {
      case (x: NumCircListCallout) :: xs if x.stack => aggregateNumCircLists(xs, acc :+ x)
      case xs => (acc, xs)
    }

  @tailrec
  private def aggregateNotes(inputSeq: Seq[VisualStanza], acc: Seq[NoteCallout]): (Seq[NoteCallout], Seq[VisualStanza]) =
    inputSeq match {
      case (x: NoteCallout) :: xs if x.stack => aggregateNotes(xs, acc :+ x)
      case xs => (acc, xs)
    }

  @tailrec
  private def aggregateYourCall(inputSeq: Seq[VisualStanza], acc: Seq[YourCallCallout]): (Seq[YourCallCallout], Seq[VisualStanza]) =
    inputSeq match {
      case (x: YourCallCallout) :: xs if x.stack => aggregateYourCall(xs, acc :+ x)
      case xs => (acc, xs)
    }

}
