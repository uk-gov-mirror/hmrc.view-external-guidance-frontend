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

import models.ocelot.stanzas.{VisualStanza, Row, RowGroup}

import scala.annotation.tailrec

object RowAggregator {

  @tailrec
  def aggregateStanzas(acc: Seq[VisualStanza])(inputSeq: Seq[VisualStanza]): Seq[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs =>
        val rowGroup: Seq[Row] = aggregate (xs, Seq (x) )
        aggregateStanzas(acc :+ RowGroup (rowGroup))(xs.drop (rowGroup.size - 1))
      case x :: xs => aggregateStanzas(acc :+ x)(xs)
    }

  @tailrec
  private def aggregate(inputSeq: Seq[VisualStanza], acc: Seq[Row]): Seq[Row] =
    inputSeq match {
      case Nil => acc
      case (x: Row) :: xs if x.stack => aggregate(xs, acc :+ x)
      case _ => acc
    }

}
