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
import play.api.libs.json.{JsPath, Reads}
import play.api.libs.json.Reads._
import models.ocelot.Phrase

import models.ocelot.Link

case class InstructionStanza(text: Int,
                             override val next: Seq[String],
                             link: Option[Int],
                             stack: Boolean) extends Stanza

object InstructionStanza {

  implicit val instructionReads: Reads[InstructionStanza] = {

    (( JsPath \ "text" ).read[Int] and
      ( JsPath \ "next" ).read[Seq[String]](minLength[Seq[String]](1)) and
      ( JsPath \ "link" ).readNullable[Int] and
      ( JsPath \ "stack" ).read[Boolean]
      ) ( InstructionStanza.apply _)

  }

}

case class Instruction(text: Phrase,
                       override val next: Seq[String],
                       link: Option[Link],
                       stack: Boolean) extends PopulatedStanza

object Instruction {
  def apply(stanza: InstructionStanza, text: Phrase, link: Option[Link] ): Instruction =
    Instruction(text, stanza.next, link, stanza.stack)
}