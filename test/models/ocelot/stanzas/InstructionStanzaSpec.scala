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

import scala.util.{Failure, Success, Try}

import org.scalatest._
import play.api.libs.json._

class InstructionStanzaSpec extends WordSpec with MustMatchers {

  val ten: Int = 10

  val validInstructionStanzaJsonInput =
    """|{
       | "type": "InstructionStanza",
       | "text": 10,
       | "next": [ "end" ],
       | "stack": false
       |}""".stripMargin

  val incorrectTypeInstructionStanzaJsonInput =
    """|{
       | "type": "InstructionStanza",
       | "text": "10",
       | "next": [ "6" ],
       | "stack": false
       |}""".stripMargin

  val validInstructionStanzaJsObject: JsObject = Json.parse( validInstructionStanzaJsonInput ).as[JsObject]

  "reading a valid Instruction Stanza should create an instance of the class InstructionStanza" in {

    val validInstructionStanzaJson: JsValue = Json.parse( validInstructionStanzaJsonInput )

    val validInstructionStanza: InstructionStanza = validInstructionStanzaJson.as[InstructionStanza]

    validInstructionStanza mustBe InstructionStanza( ten, Seq( "end" ), stack = false )
  }

  /* Test for missing properties in Json object */
  validInstructionStanzaJsObject.keys.filterNot( attributeName => attributeName == "type" ).foreach { attributeName =>
    s"throw an exception when Json object is missing the $attributeName attribute" in {
      val invalidJson = validInstructionStanzaJsObject - attributeName
      invalidJson.validate[InstructionStanza] match {
        case JsSuccess(_,_) => fail( s"InstructionStanza created when $attributeName is missing" )
        case JsError(_) => succeed
      }
    }
  }

  "reading an InstructionStanza with a property of the wrong type should cause an exception to be raised" in {

    val incorrectTypeInstructionStanzaJson: JsValue = Json.parse( incorrectTypeInstructionStanzaJsonInput )

    Try
    {
      val instructionStanza = incorrectTypeInstructionStanzaJson.as[InstructionStanza]
    }
    match {
      case Success(_) => fail( "An instruction stanza should not be created from Json with an incorrect property type" )
      case Failure( failure ) => println( "Failure message : " + failure.getMessage )
    }
  }

}
