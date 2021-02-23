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

// FlowStack: Flow, Flow, Flow, Continuation, Flow, Flow, Flow, Continuation

// Continuation: continuation next and post sequence, non-visual stanzas from page

// The PageRenderer will add the current Continuation stanzas into this stanzaMap when Continuation followed


package core.models.ocelot

import play.api.libs.functional.syntax._
import play.api.libs.json._

sealed trait FlowStage {
  val next: String
}

case class LabelValue(name: String, value: Option[String])
final case class Flow(next: String, labelValue: Option[LabelValue]) extends FlowStage
final case class Continuation(next: String, stanzas: List[KeyedStanza]) extends FlowStage


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

object Flow {
  implicit val reads: Reads[Flow] = (
    (__ \ "next").read[String] and
      (__ \ "labelValue").readNullable[LabelValue]
  )(Flow.apply _)

  implicit val writes: Writes[Flow] = (
    (__ \ "next").write[String] and
      (__ \ "labelValue").writeNullable[LabelValue]
  )(unlift(Flow.unapply))
}

object Continuation {
  implicit val reads: Reads[Continuation] = (
    (__ \ "next").read[String] and
      (__ \ "stanzas").read[List[KeyedStanza]]
  )(Continuation.apply _)

  implicit val writes: Writes[Continuation] = (
    (__ \ "next").write[String] and
      (__ \ "stanzas").write[List[KeyedStanza]]
  )(unlift(Continuation.unapply))
}

