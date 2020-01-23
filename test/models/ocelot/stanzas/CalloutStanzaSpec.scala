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

import org.scalatest._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

class CalloutStanzaSpec extends WordSpec with MustMatchers {

  val ten: Int = 10

  val titleCalloutStanzaInputJson =
      """|{
         |    "type": "CalloutStanza",
         |    "noteType": "Title",
         |    "text": 0,
         |    "next": ["1"],
         |    "stack": false
         |}""".stripMargin

  val subTitleCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Subtitle",
       |    "text": 1,
       |    "next": ["2"],
       |    "stack": true
       |}""".stripMargin

  val ledeCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Lede",
       |    "text": 2,
       |    "next": ["3"],
       |    "stack": false
       |}""".stripMargin

  val errorCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Error",
       |    "text": 10,
       |    "next": ["end"],
       |    "stack": false
       |}""".stripMargin

  val invalidCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Invalid",
       |    "text": 10,
       |    "next": ["end"],
       |    "stack": false
       |}""".stripMargin


  val missingPropertyCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Title",
       |    "text": 0,
       |    "stack": false
       |}""".stripMargin

  val incorrectTypeCalloutStanzaInputJson =
    """|{
       |    "type": "CalloutStanza",
       |    "noteType": "Title",
       |    "text": 0,
       |    "next": [1],
       |    "stack": false
       |}""".stripMargin

  "read valid Callout stanza of type Title should create a Callout stanza with a note type of Title" in {

    val titleCalloutStanzaJson: JsValue = Json.parse( titleCalloutStanzaInputJson )

    val titleCalloutStanza: CalloutStanza = titleCalloutStanzaJson.as[CalloutStanza]

    titleCalloutStanza mustBe CalloutStanza( Title, 0, Seq( "1"), stack = false )
  }

  "read valid Callout stanza of type Subtitle should create a Callout stanza with a note type of Subtitle" in {

    val subTitleCalloutStanzaJson : JsValue = Json.parse( subTitleCalloutStanzaInputJson )

    val subTitleCalloutStanza: CalloutStanza = subTitleCalloutStanzaJson.as[CalloutStanza]

    subTitleCalloutStanza mustBe CalloutStanza( SubTitle, 1, Seq( "2"), stack = true )
  }

  "read valid Callout stanza of type Lede should create a Callout stanza with a note type of Lede" in {

    val ledeCalloutStanzaJson : JsValue = Json.parse( ledeCalloutStanzaInputJson )

    val ledeCalloutStanza: CalloutStanza = ledeCalloutStanzaJson.as[CalloutStanza]

    ledeCalloutStanza mustBe CalloutStanza( Lede, 2, Seq( "3" ), stack = false )
  }

  "read valid Callout stanza of type Error should create a Callout stanza with a note type of Error" in {

    val errorCalloutStanzaJson : JsValue = Json.parse( errorCalloutStanzaInputJson )

    val errorCalloutStanza: CalloutStanza = errorCalloutStanzaJson.as[CalloutStanza]

    errorCalloutStanza mustBe CalloutStanza( Error, ten, Seq( "end" ), stack = false )
  }

  "read Callout stanza with invalid note type should cause an exception to be raised" in {

    val invalidCalloutStanzaJson = Json.parse( invalidCalloutStanzaInputJson )

    Try{
      val calloutStanza : CalloutStanza = invalidCalloutStanzaJson.as[CalloutStanza]
    }
    match {
      case Success(_) => fail( "An instance of CalloutStanza should not be created when the note type is incorrect" )
      case Failure( failure ) => println( "Failure message : " + failure.getMessage )
    }
  }

  "read Callout stanza with missing property should cause an exception to be raised" in {

    val missingPropertyCalloutStanzaJson: JsValue = Json.parse( missingPropertyCalloutStanzaInputJson )

    Try
    {
      val calloutStanza: CalloutStanza = missingPropertyCalloutStanzaJson.as[CalloutStanza]
    }
    match {
      case Success(_) => fail( "An instance of CalloutStanza should not be created from incomplete Json" )
      case Failure( failure ) => println( "Failure message : " + failure.getMessage )
    }

  }

  "read Callout stanza with incorrect property type cause an exception to be raised" in {

    val incorrectTypeCalloutStanzaJson: JsValue = Json.parse( incorrectTypeCalloutStanzaInputJson )

    Try
    {
      val calloutStanza: CalloutStanza = incorrectTypeCalloutStanzaJson.as[CalloutStanza]
    }
    match {
      case Success(_) => fail( "An instance of CalloutStanza should not be created from Json with incorrect property type" )
      case Failure( failure ) => println( s"Failure message: ${failure.getMessage}" )
    }

  }

}
