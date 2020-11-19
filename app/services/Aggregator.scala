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

import models.ocelot.stanzas.{VisualStanza, Row, RowGroup, NumListCallout, NumCircListCallout, NumListGroup, NumCircListGroup}

import scala.annotation.tailrec

object Aggregator {

  @tailrec
  def aggregateStanzas(acc: Seq[VisualStanza])(inputSeq: Seq[VisualStanza]): Seq[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs =>
        val rowGroup: Seq[Row] = aggregateRows(xs, Seq (x))
        aggregateStanzas(acc :+ RowGroup (rowGroup))(xs.drop (rowGroup.size - 1))
      case (x: NumListCallout) :: xs =>
        val nlCallouts: Seq[NumListCallout] = aggregateNumLists(xs, Seq (x))
        aggregateStanzas(acc :+ NumListGroup(nlCallouts))(xs.drop (nlCallouts.size - 1))
      case (x: NumCircListCallout) :: xs =>
        val nclCallouts: Seq[NumCircListCallout] = aggregateNumCircLists(xs, Seq (x))
        aggregateStanzas(acc :+ NumCircListGroup(nclCallouts))(xs.drop (nclCallouts.size - 1))
      case x :: xs => aggregateStanzas(acc :+ x)(xs)
    }

  @tailrec
  private def aggregateRows(inputSeq: Seq[VisualStanza], acc: Seq[Row]): Seq[Row] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs if x.stack => aggregateRows(xs, acc :+ x)
      case _ => acc
    }

  @tailrec
  private def aggregateNumLists(inputSeq: Seq[VisualStanza], acc: Seq[NumListCallout]): Seq[NumListCallout] =
    inputSeq match {
      case Nil => acc
      case (x: NumListCallout) :: xs if x.stack => aggregateNumLists(xs, acc :+ x)
      case _ => acc
    }

  @tailrec
  private def aggregateNumCircLists(inputSeq: Seq[VisualStanza], acc: Seq[NumCircListCallout]): Seq[NumCircListCallout] =
    inputSeq match {
      case Nil => acc
      case (x: NumCircListCallout) :: xs if x.stack => aggregateNumCircLists(xs, acc :+ x)
      case _ => acc
    }

}
