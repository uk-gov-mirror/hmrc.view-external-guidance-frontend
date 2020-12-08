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
//
// 1. Cell content all bold encoded within a <th>
// 2. Cell content not bold encoded within a <td>
// 3. If First Row all cells non-blank and all bold encode as <thead> containing row of <th> elements
// 4. If group of rows is stacked to a SubSection Callout, encode callout text as a <caption> within <table>
// 5. If cell contents numeric (lable ref with output format currency), add the govuk-table__cell--numeric class
//
// Not stacked and first row not a thead => plain table of <tr> elements wher cells follow rule 1

case class Table(caption: Text, headingRow: Seq[Text], rows: Seq[Seq[Text]]) extends UIComponent {
  val numericColumns: Seq[Boolean] = (for(colIdx <- headingRow.indices) yield {
    rows.map(r => r(colIdx).isNumericLabelRef)
  }).map(l => l.forall(x => x))

  val text: Text = caption
}