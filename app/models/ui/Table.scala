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

trait Cell {
  val text: Text
  val numeric: Boolean
}
case class Th(text: Text, numeric: Boolean = false) extends Cell
case class Td(text: Text, numeric: Boolean = false) extends Cell


case class Table(caption: Option[Text], headingRow: Option[Seq[Cell]], rows: Seq[Seq[Cell]]) extends UIComponent {
  val text: Text = caption.fold(Text())(t => t)
}