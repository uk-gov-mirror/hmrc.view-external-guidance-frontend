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

package core.models.ocelot

import base.BaseSpec
import play.api.libs.json._

class FlowSpec extends BaseSpec {

  val labelValueJson: JsValue = Json.parse(
    s"""|{
        |    "name": "LabelName",
        |    "value": "A value"
        |}""".stripMargin
  )

  val invalidLabelValueJson: JsValue = Json.parse(
    s"""|{
        |    "namef": "LabelName",
        |    "value": "A value"
        |}""".stripMargin
  )

  val flowJson: JsValue = Json.parse(
    s"""|{
        |    "next": "1",
        |    "labelValue": {
        |     "name": "LabelName",
        |     "value": "A value"
        |    }
        |}""".stripMargin
  )

  val invalidFlowJson: JsValue = Json.parse(
    s"""|{
        |    "nextf": "1",
        |    "labelValue": {
        |     "name": "LabelName",
        |     "value": "A value"
        |    }
        |}""".stripMargin
  )


  val expectedLabelValue: LabelValue = LabelValue("LabelName", Some("A value"))
  val expectedFlow: Flow = Flow("1", Some(expectedLabelValue))


  "Reading valid Flow JSON" should {
    "create a Flow" in {
      flowJson.as[Flow] shouldBe expectedFlow
    }
  }

  "Reading invalid Flow JSON" should {
    "generate a JsError" in {
      invalidFlowJson.validate[Flow] match {
        case JsError(_) => succeed
        case _ => fail("An instance of Flow should not be created next is missing")
      }
    }
  }

  "serialise Flow to json" in {
    Json.toJson(expectedFlow) shouldBe flowJson
  }

  "Reading valid LabelValue JSON" should {
    "create a LabelValue" in {
      labelValueJson.as[LabelValue] shouldBe expectedLabelValue
    }
  }

  "Reading invalid LabelValue JSON" should {
    "generate a JsError" in {
      invalidLabelValueJson.validate[LabelValue] match {
        case JsError(_) => succeed
        case _ => fail("An instance of LabelValue should not be created next is missing")
      }
    }
  }

  "serialise LabelValue to json" in {
    Json.toJson(expectedLabelValue) shouldBe labelValueJson
  }

}
