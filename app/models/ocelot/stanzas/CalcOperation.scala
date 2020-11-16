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

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

import models.ocelot.{asAnyInt, labelReference}
import models.ocelot.errors.{InvalidScaleFactorError, GuidanceError}

case class CalcOperation(left:String, op: CalcOperationType, right: String, label: String) {

  def validate(stanzaId: String): Option[GuidanceError] =
    op match {
      case Ceiling => validateScaleFactor(stanzaId, right)
      case Floor => validateScaleFactor(stanzaId, right)
      case _ => None
    }

  private def validateScaleFactor(stanzaId: String, scale: String) : Option[GuidanceError] = {

    if(labelReference(scale).isDefined) {
      None
    } else if(asAnyInt(scale).isDefined) None else Some(InvalidScaleFactorError(stanzaId, "Invalid scale factor in ceiling or floor calculation operation"))

  }

}

object CalcOperation {

  implicit val reads: Reads[CalcOperation] =
    (
      (JsPath \ "left").read[String] and
        (JsPath \ "op").read[CalcOperationType] and
        (JsPath \ "right").read[String] and
        (JsPath \ "label").read[String]
    )(CalcOperation.apply _)

  implicit val writes: OWrites[CalcOperation] =
    (
      (JsPath \ "left").write[String] and
        (JsPath \ "op").write[CalcOperationType] and
        (JsPath \ "right").write[String] and
        (JsPath \ "label").write[String]
    )(unlift(CalcOperation.unapply))
}
