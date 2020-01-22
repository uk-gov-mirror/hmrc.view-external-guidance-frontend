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

import org.scalatest._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

class CalloutStanzaSpec extends FlatSpec {

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

  "read valid Callout stanza of type Title" should "Create a Callout stanza with a note type of Title" in {

    val titleCalloutStanzaJson: JsValue = Json.parse( titleCalloutStanzaInputJson )

    val titleCalloutStanza: CalloutStanza = titleCalloutStanzaJson.as[CalloutStanza]

    assert( titleCalloutStanza == CalloutStanza( "Title", 0, Seq( "1"), false ) )

  }

  "read valid Callout stanza of type subtitle" should "Create a Callout stanza with a note type of Subtitle" in {

    val subTitleCalloutStanzaJson : JsValue = Json.parse( subTitleCalloutStanzaInputJson )

    val subTitleCalloutStanza: CalloutStanza = subTitleCalloutStanzaJson.as[CalloutStanza]

    assert( subTitleCalloutStanza == CalloutStanza( "Subtitle", 1, Seq( "2"), true ) )
  }

  "read Callout stanza with missing property" should "Cause an exception to be raised" in {

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

  "read Callout stanza with incorrect property type" should "Cause an exception to be raised" in {

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
