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


case class NumberedList(override val next: Seq[String], group: Seq[NumberedListItemCallout], stack: Boolean) extends VisualStanza with Populated

object NumberedList {
  def apply(group: Seq[NumberedListItemCallout]): NumberedList = NumberedList(group.last.next, group, group.head.stack)
}

case class NumberedCircleList(override val next: Seq[String], group: Seq[NumberedCircleListItemCallout], stack: Boolean) extends VisualStanza with Populated

object NumberedCircleList {
  def apply(group: Seq[NumberedCircleListItemCallout]): NumberedCircleList = NumberedCircleList(group.last.next, group, group.head.stack)
}