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

class QuestionStanzaSpec extends WordSpec with MustMatchers {

  val four: Int = 4
  val five: Int = 5

  val twoAnswersQuestionStanzaJsonInput =
    """|{
       | "type" : "QuestionStanza",
       | "text": 0,
       | "answers": [ 0, 2 ],
       | "next": [ "2", "5" ],
       | "stack": false
       |}""".stripMargin

  val threeAnswersQuestionStanzaJsonInput =
    """|{
       | "type": "QuestionStanza",
       | "text": 1,
       | "answers": [ 3, 4, 5 ],
       | "next": [ "4", "7", "8" ],
       | "stack": false
       |}""".stripMargin

  val incorrectTypeQuestionStanzaJsonInput =
    """|{
       | "type": "QuestionStanza",
       | "text": 1,
       | "answers": [ 3, 4, 5 ],
       | "next": [ "4", "7", "8" ],
       | "stack": 0
       |}""".stripMargin

  val validQuestionStanzaJson: JsObject = Json.parse( twoAnswersQuestionStanzaJsonInput ).as[JsObject]

  "Question stanza" must {

    "reading a valid QuestionStanza with two answers should create an instance of the class QuestionStanza" in {

      val twoAnswersQuestionStanzaJson: JsValue = Json.parse(twoAnswersQuestionStanzaJsonInput)

      val twoAnswersQuestionStanza: QuestionStanza = twoAnswersQuestionStanzaJson.as[QuestionStanza]

      twoAnswersQuestionStanza mustBe QuestionStanza(0, Seq(0, 2), Seq("2", "5"), stack = false)
    }

    "reading a valid QuestionStanza with three answers should create an instance of the class QuestionStanza" in {

      val threeAnswersQuestionStanzaJson: JsValue = Json.parse(threeAnswersQuestionStanzaJsonInput)

      val threeAnswersQuestionStanza: QuestionStanza = threeAnswersQuestionStanzaJson.as[QuestionStanza]

      threeAnswersQuestionStanza mustBe QuestionStanza(1, Seq(3, four, five), Seq("4", "7", "8"), false)
    }

    /** Test for missing properties in Json object */
    validQuestionStanzaJson.keys.filterNot( attributeName => attributeName == "type" ).foreach { attributeName =>
      s"throw an exception when json is missing the $attributeName attribute" in {
        val invalidJson = validQuestionStanzaJson - attributeName
        invalidJson.validate[QuestionStanza] match {
          case JsSuccess(_, _) => fail(s"QuestionStanza object created when $attributeName attribute is missing")
          case JsError(_) => succeed
        }
      }
    }

    "reading a QuestionStanza with a property of incorrect type should cause an exception to be raised" in {

      val incorrectTypeQuestionStanzaJson: JsValue = Json.parse(incorrectTypeQuestionStanzaJsonInput)

      Try {
        val questionStanza: QuestionStanza = incorrectTypeQuestionStanzaJson.as[QuestionStanza]
      }
      match {
        case Success(_) => fail("An instance of QuestionStanza should not have been created from Json with an incorrect property type")
        case Failure(failure) => println("Failure message for incorrect property type : " + failure.getMessage)
      }

    }

  }

}
