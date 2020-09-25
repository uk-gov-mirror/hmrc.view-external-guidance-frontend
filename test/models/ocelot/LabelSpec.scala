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

import base.BaseSpec
import play.api.libs.json._
import models.ocelot.stanzas._

class LabelSpec extends BaseSpec with ProcessJson {


  val label = """{"name":"BLAH"}"""
  val labelWithValue = """{"name":"BLAH","value":"39.99"}"""
  val labelWithType = """{"name":"BLAH","valueType":"Currency"}"""
  val labelWithValueAndType = """{"name":"BLAH","value":"32.99","valueType":"Currency"}"""

  "Label" must {
    "deserialise " in {
      Json.parse(label).as[Label] shouldBe Label("BLAH")
      Json.parse(labelWithValue).as[Label] shouldBe Label("BLAH", Some("39.99"))
      Json.parse(labelWithType).as[Label] shouldBe Label("BLAH", None, Some(Currency))
      Json.parse(labelWithValueAndType).as[Label] shouldBe Label("BLAH", Some("32.99"), Some(Currency))
    }

    "serialise from Label to json" in {
      Json.toJson(Label("BLAH")).toString shouldBe label
      Json.toJson(Label("BLAH", Some("39.99"))).toString shouldBe labelWithValue
      Json.toJson(Label("BLAH", None, Some(Currency))).toString shouldBe labelWithType
      Json.toJson(Label("BLAH", Some("32.99"), Some(Currency))).toString shouldBe labelWithValueAndType
    }

  }

}
