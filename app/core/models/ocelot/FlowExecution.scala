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

package core.models.ocelot

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class LabelValue(name: String, value: Option[String])
case class FlowExecution(next: String, labelValue: Option[LabelValue])

object LabelValue {
  implicit val reads: Reads[LabelValue] = (
    (__ \ "name").read[String] and
      (__ \ "value").readNullable[String]
  )(LabelValue.apply _)

  implicit val writes: Writes[LabelValue] = (
    (__ \ "name").write[String] and
      (__ \ "value").writeNullable[String]
  )(unlift(LabelValue.unapply))
}

object FlowExecution {
  implicit val reads: Reads[FlowExecution] = (
    (__ \ "next").read[String] and
      (__ \ "labelValue").readNullable[LabelValue]
  )(FlowExecution.apply _)

  implicit val writes: Writes[FlowExecution] = (
    (__ \ "next").write[String] and
      (__ \ "labelValue").writeNullable[LabelValue]
  )(unlift(FlowExecution.unapply))
}
