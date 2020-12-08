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

// $COVERAGE-OFF$

package models.ui

sealed trait SummaryList extends UIComponent {
  val columnCount: Int
  val text = Text()
  val rows: Seq[Seq[Text]]
  val numericColumns: Seq[Boolean]
}

case class CyaSummaryList(val rows: Seq[Seq[Text]], columnCount: Int) extends SummaryList {
  val numericColumns: Seq[Boolean] = Seq.fill(columnCount)(false)
}

case class NameValueSummaryList(val rows: Seq[Seq[Text]], columnCount: Int) extends SummaryList {
  lazy val numericColumns: Seq[Boolean] = (for(colIdx <- Range(0, columnCount)) yield {
    rows.map(r => r(colIdx).isNumericLabelRef)
  }).map(l => l.forall(x => x))
}