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

case class Value(valueType:ValueType, label: String, value: String)

object Value {
  implicit val reads: Reads[Value] =
    ((__ \ "type").read[ValueType] and
     (__ \ "label").read[String] and
     (__ \ "value").read[String]) (Value.apply _)

}

case class ValueStanza(values: List[Value], next: Seq[String], stack: Boolean) extends Stanza

object ValueStanza {
  implicit val reads: Reads[ValueStanza] =
    ((__ \ "values").read[List[Value]] and
     (__ \ "next").read[Seq[String]] and
     (__ \ "stack").read[Boolean] ) (ValueStanza.apply _)

}