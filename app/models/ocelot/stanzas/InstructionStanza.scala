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
                       stack: Boolean,
                       linkedStanzaIds: List[String] = Nil) extends PopulatedStanza

object Instruction {
  private val pageLinkRegex = """\[link:.+?:(\d+)\]""".r

  def isInteger(s: String): Boolean = s.forall(_.isDigit)

  def apply(stanza: InstructionStanza, text: Phrase, link: Option[Link]): Instruction = {
    val linkedStanzaId: List[String] = link.map(lnk => List(lnk.dest)).filter(l => isInteger(l(0))).getOrElse(Nil)
    val placeholderLinkedStanzaIds: List[String] = pageLinkRegex.findAllMatchIn(text.langs(0)).map(_.group(1)).toList
    val stanzaIds: List[String] = (placeholderLinkedStanzaIds ++ linkedStanzaId).distinct
    Instruction(text, stanza.next, link, stanza.stack, stanzaIds)
  }
}
