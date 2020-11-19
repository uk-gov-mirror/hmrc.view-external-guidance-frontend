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

import models.ocelot.asAnyInt

case class CalcOperation(left:String, op: CalcOperationType, right: String, label: String)

object CalcOperation {

  val parseError: String = "error.path.missing.or.property.type.incorrect"

  implicit val reads:Reads[CalcOperation] = new Reads[CalcOperation] {

    def reads(json: JsValue): JsResult[CalcOperation] = {

        json match {
          case jsValue: JsValue => {

            val leftOpt: Option[String] = (jsValue \ "left").asOpt[String]
            val opOpt: Option[CalcOperationType] = (jsValue \ "op").asOpt[CalcOperationType]
            val rightOpt: Option[String] = (jsValue \ "right").asOpt[String]
            val labelOpt: Option[String] = (jsValue \ "label").asOpt[String]

            populateCalcOperation(json, leftOpt, opOpt, rightOpt, labelOpt)
          }
          case _ => JsError(Seq(JsPath() -> Seq(JsonValidationError(Seq("error", "error.expected.jsobject")))))
        }

    }

    private def populateCalcOperation(
                                       jsValue: JsValue,
                                       leftOpt: Option[String],
                                       opOpt: Option[CalcOperationType],
                                       rightOpt: Option[String],
                                       labelOpt: Option[String]): JsResult[CalcOperation] = {

      leftOpt match {
        case Some(left) => {
          opOpt match {
            case Some(op) => {
              rightOpt match {
                case Some(right) => {
                  labelOpt match {
                    case Some(label) => {
                      // All required values have been defined so we can apply custom validation
                      validateOp(left, op, right, label)
                    }
                    case None => JsError(Seq(JsPath \ "right" -> Seq(JsonValidationError(Seq(parseError)))))
                  }
                }
                case None => JsError(Seq(JsPath \ "right" -> Seq(JsonValidationError(Seq(parseError)))))
              }
            }
            case None => handleUndefinedCalculationOperationType(jsValue, opOpt)
          }
        }
        case None => JsError(Seq(JsPath \ "left" -> Seq(JsonValidationError(Seq(parseError)))))
      }

    }

    private def validateOp(left: String, op: CalcOperationType, right: String, label: String): JsResult[CalcOperation] = {

      op match {
        case Ceiling | Floor => {
          asAnyInt(right) match {
            case Some(value) => JsSuccess(CalcOperation(left, op, right, label))
            case None => JsError(Seq(JsPath \ "op" -> Seq(JsonValidationError(Seq("error", "error.noninteger.scalefactor")))))
          }
        }
        case _ => JsSuccess(CalcOperation(left, op, right, label))
      }

    }

    private def handleUndefinedCalculationOperationType(jsValue: JsValue, opOpt: Option[CalcOperationType]): JsError = {

      val opAsString: Option[String] = (jsValue \ "op").asOpt[String]

      opAsString match {
        case Some(value) =>  JsError(Seq(JsPath \ "op" -> Seq(JsonValidationError(Seq("CalcOperationType"), value))))
        case None => JsError(Seq(JsPath \ "op" -> Seq(JsonValidationError(Seq(parseError)))))
      }

    }

  }

  implicit val writes: OWrites[CalcOperation] =
    (
      (JsPath \ "left").write[String] and
        (JsPath \ "op").write[CalcOperationType] and
        (JsPath \ "right").write[String] and
        (JsPath \ "label").write[String]
    )(unlift(CalcOperation.unapply))

}
