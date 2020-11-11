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

package models.ocelot

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.i18n.Lang

trait Label {
  val name: String
  val value: Option[String]
  def displayValue(implicit lang: Lang): Option[String] = value
}

case class ValueLabel(name: String, value: Option[String] = None) extends Label

object ValueLabel {
  implicit val reads: Reads[ValueLabel] = (
    (__ \ "name").read[String] and
      (__ \ "value").readNullable[String]
  )(ValueLabel.apply _)

  implicit val writes: OWrites[ValueLabel] = (
    (__ \ "name").write[String] and
      (__ \ "value").writeNullable[String]
  )(unlift(ValueLabel.unapply))
}

case class DisplayLabel(name: String, english: Option[String] = None, welsh: Option[String] = None) extends Label {
  override val value: Option[String] = english
  override def displayValue(implicit lang: Lang): Option[String] = lang.code match {
    case "en" => english
    case "cy" => welsh
  }
}

object DisplayLabel {
  implicit val reads: Reads[DisplayLabel] = (
    (__ \ "name").read[String] and
      (__ \ "english").readNullable[String] and
      (__ \ "welsh").readNullable[String]
  )(DisplayLabel.apply _)

  implicit val writes: OWrites[DisplayLabel] = (
    (__ \ "name").write[String] and
      (__ \ "english").writeNullable[String] and
      (__ \ "welsh").writeNullable[String]
  )(unlift(DisplayLabel.unapply))
}

object Label {
  def apply(lbl: Label, value: Option[String]): Label = ValueLabel(lbl.name, value)
  def apply(lbl: Label, english: Option[String], welsh: Option[String]): Label = DisplayLabel(lbl.name, english, welsh)

  implicit val reads: Reads[Label] = (js: JsValue) => {
    (js \ "type").validate[String] match {
      case err @ JsError(_) => err
      case JsSuccess("Value", _) => js.validate[ValueLabel]
      case JsSuccess("Display", _) => js.validate[DisplayLabel]
      case JsSuccess(typeName, _) => JsError(JsonValidationError(Seq("Stanza"), typeName))
    }
  }

  implicit val writes: Writes[Label] = {
    case v: ValueLabel => Json.obj("type" -> "Value") ++ Json.toJsObject[ValueLabel](v)
    case d: DisplayLabel => Json.obj("type" -> "Display") ++ Json.toJsObject[DisplayLabel](d)
    case s => Json.toJson("")
  }
}
