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

package models.ocelot.stanzas

import models.ocelot.isLinkOnlyPhrase
import models.ocelot.Phrase

case class RowGroup (override val next: Seq[String], group: Seq[Row], stack: Boolean) extends VisualStanza with Populated {
  lazy val maxRowLength:Int = group.headOption.fold(0)(_ => group.map(_.cells.length).max)
  lazy val paddedRows = group.map(row => (row.cells ++ Seq.fill(maxRowLength - row.cells.size)(Phrase())))
  lazy val isSummaryList: Boolean = maxRowLength == 3 &&
                                    group.forall(r => r.cells.length < 3 ||
                                                      isLinkOnlyPhrase(r.cells(2)) ||
                                                      r.cells(2) == Phrase())
}

object RowGroup {
  def apply(group: Seq[Row]): RowGroup =
    group match {
      case Nil => RowGroup(Seq.empty, Seq.empty, false)
      case _ => RowGroup(group.last.next, group, group.head.stack)
    }
}