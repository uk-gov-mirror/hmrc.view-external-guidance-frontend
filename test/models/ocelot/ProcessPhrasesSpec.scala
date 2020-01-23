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


import org.scalatestplus.play.PlaySpec
import scala.util.{Try,Success,Failure}
import play.api.libs.json._
import base.ProcessJson


class ProcessPhrasesSpec extends PlaySpec with ProcessJson {

  val validPhrasesSectionJson: JsValue = Json.parse(
    """{
      |  "phrases": [
      |    ["Ask the customer if they have a tea bag", "Welsh, Ask the customer if they have a tea bag"],
      |    ["Do you have a tea bag?", "Welsh, Do you have a tea bag?"],
      |    ["Yes - they do have a tea bag", "Welsh, Yes - they do have a tea bag"]
      |  ]
      |}""".stripMargin
  )

  val invalidPhrasesSectionJson: JsValue = Json.parse(
    """{
      |  "phrases": [
      |    ["Ask the customer if they have a tea bag", "Welsh, Ask the customer if they have a tea bag"],
      |    ["Do you have a tea bag?"],
      |    ["Yes - they do have a tea bag", "Welsh, Yes - they do have a tea bag"]
      |  ]
      |}""".stripMargin
  )

  "Phrases section of Ocelet process" must {

    "deserialise from phrases section json" in {

      val result: Phrases = validPhrasesSectionJson.as[Phrases]

      result mustBe Phrases(
                      Seq(
                      Phrase("Ask the customer if they have a tea bag", "Welsh, Ask the customer if they have a tea bag"),
                      Phrase("Do you have a tea bag?", "Welsh, Do you have a tea bag?"),
                      Phrase("Yes - they do have a tea bag", "Welsh, Yes - they do have a tea bag")
                      )
                    )
    }

    "should allow access to phrase by index" in {

      val phrases: Phrases = validPhrasesSectionJson.as[Phrases]

      phrases.elems(1).welsh mustBe "Welsh, Do you have a tea bag?"

      phrases.phrase(1).map{ phr =>
        phr.welsh mustBe "Welsh, Do you have a tea bag?"
      } orElse fail("Missing phrase at index 1")
    }

   "throw exception when json invalid" in {

      Try {
        invalidPhrasesSectionJson.as[Meta]
      } match {
        case Success(_) => fail("Invalid json should not create a Phrases object")
        case Failure(_) =>
      }
    }
  }

}
