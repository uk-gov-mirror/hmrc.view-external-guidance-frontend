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
import play.api.libs.functional.syntax._

sealed trait Stanza


/**
 * Define Callout Stanza
 */
case class CalloutStanza(
                        noteType: String,
                        text: Int,
                        next: Seq[String],
                        stack: Boolean
                        ) extends Stanza

object CalloutStanza {

  implicit val calloutReads: Reads[CalloutStanza] = {

    (( JsPath \ "noteType" ).read[String] and
      ( JsPath \ "text" ).read[Int] and
      ( JsPath \ "next" ).read[Seq[String]] and
      ( JsPath \ "stack" ).read[Boolean]
      )(CalloutStanza.apply _)

  }

}

/**
 * Define Instruction Stanza
 */
case class InstructionStanza(
                            text: Int,
                            next: Seq[String],
                            stack: Boolean
                            ) extends Stanza

object InstructionStanza {

  implicit val instructionReads: Reads[InstructionStanza] = {

    (( JsPath \ "text" ).read[Int] and
      ( JsPath \ "next" ).read[Seq[String]] and
      ( JsPath \ "stack" ).read[Boolean]
      ) ( InstructionStanza.apply _)

  }

}

