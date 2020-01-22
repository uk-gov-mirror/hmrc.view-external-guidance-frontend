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


// import play.api.libs.json._
// import play.api.libs.functional.syntax._
import org.scalatestplus.play.PlaySpec

import scala.util.{Try,Success,Failure}
import play.api.libs.json.{JsonValidationError, _}
import base.ProcessJson


class ProcessMetaSpec extends PlaySpec with ProcessJson {

  val validMetaSectionJson: JsValue = Json.parse(
    """{
      |  "meta": {
      |   "title": "Customer wants to make a cup of tea",
      |   "id": "oct90001",
      |   "ocelot": 1,
      |   "lastAuthor": "0000023",
      |   "lastUpdate": 1500298931016,
      |   "version": 4,
      |   "filename": "oct90001.js"
      |  }
      |}""".stripMargin
  )

  val invalidMetaSectionJson: JsValue = Json.parse(
    """{
      | "meta":{
      |  "title": "Customer wants to make a cup of tea",
      |  "id": "oct90001",
      |  "lastAuthor": "000000",
      |  "lastUpdate": 1500298931016,
      |  "filename": "oct90001.js"
      | }
      |}""".stripMargin
  )

  "Meta section of Ocelet process" must {

    "deserialise from meta section json" in {

      val result: Meta = validMetaSectionJson.as[Meta]

      result mustBe Meta("oct90001","Customer wants to make a cup of tea",1,"0000023", 1500298931016L, 4, "oct90001.js")
    }

    "deserialise from process json" in {

      val result: Meta = fullJson.as[Meta]

      result mustBe Meta("ext90002","Telling HMRC about extra income",1,"6031631", 1579177321336L, 1, "ext90002.js")
    }

   "throw exception when json invalid" in {

      Try {
        invalidMetaSectionJson.as[Meta]
      } match {
        case Success(_) => fail("Invalid json should not create a Meta object")
        case Failure(_) =>
      }
    }
  }

}
