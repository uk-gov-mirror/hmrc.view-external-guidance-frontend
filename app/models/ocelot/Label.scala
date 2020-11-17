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

case class Label(name: String, english: Option[String] = None, welsh: Option[String] = None)

object Label {
  implicit val reads: Reads[Label] = (
    (__ \ "name").read[String] and
      (__ \ "english").readNullable[String] and
      (__ \ "welsh").readNullable[String]
  )(Label.apply _)

  implicit val writes: Writes[Label] = (
    (__ \ "name").write[String] and
      (__ \ "english").writeNullable[String] and
      (__ \ "welsh").writeNullable[String]
  )(unlift(Label.unapply))
}