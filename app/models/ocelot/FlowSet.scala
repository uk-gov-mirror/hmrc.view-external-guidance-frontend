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

package models.ocelot

import core.models.ocelot.Labels

case class LabelSequence(name: String, values: List[String])
case class LabelValue(name: String, value: Option[String])

case class FlowSet(next: Seq[String], labelSet: Option[LabelSequence]) {
  def nextFlow(labels: Labels): (Option[(String, Option[LabelValue])], Labels) = ???
}
