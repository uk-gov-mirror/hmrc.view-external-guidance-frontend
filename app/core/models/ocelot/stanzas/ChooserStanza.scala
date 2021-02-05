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

package core.models.ocelot.stanzas

import core.models.ocelot.{Labels, Label, Phrase}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsPath, OWrites, Reads}

case class ChooserStanza(text: Int,
                         override val next: Seq[String],
                         source: String,
                         min: Int,
                         max: Int,
                         label: String,
                         stack: Boolean) extends VisualStanza

object ChooserStanza {
  implicit val reads: Reads[ChooserStanza] =
    ((JsPath \ "text").read[Int] and
      (JsPath \ "next").read[Seq[String]](minLength[Seq[String]](1)) and
      (JsPath \ "source").read[String] and
      (JsPath \ "min").read[Int] and
      (JsPath \ "max").read[Int] and
      (JsPath \ "label").read[String] and
      (JsPath \ "stack").read[Boolean])(ChooserStanza.apply _)

  implicit val writes: OWrites[ChooserStanza] =
    (
      (JsPath \ "text").write[Int] and
        (JsPath \ "next").write[Seq[String]] and
        (JsPath \ "source").write[String] and
        (JsPath \ "min").write[Int] and
        (JsPath \ "max").write[Int] and
        (JsPath \ "label").write[String] and
        (JsPath \ "stack").write[Boolean]
    )(unlift(ChooserStanza.unapply))
}

case class Chooser(text: Phrase,
                   override val next: Seq[String],
                   source: String,
                   min: Int,
                   max: Int,
                   label: String,
                   stack: Boolean) extends VisualStanza with Populated with DataInput {
  override val labelRefs: List[String] = ???
  override val labels: List[Label] = ???

  def eval(value: String, labels: Labels): (Option[String], Labels) = ???
  def validInput(value: String): Option[String] = ???
}