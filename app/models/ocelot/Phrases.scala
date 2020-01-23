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

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class Phrase(english: String, welsh: String)
case class Phrases(msgs: Seq[Phrase])

object Phrases {
  implicit val readsPhrase: Reads[Phrase] = __.read[Seq[String]](minLength[Seq[String]](2)).map( s => Phrase(s(0), s(1)) )
  implicit val readsPhrases: Reads[Phrases] = (__ \ "phrases").read[Seq[Phrase]].map(Phrases(_))
}
