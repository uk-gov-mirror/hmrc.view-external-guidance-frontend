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


// import play.api.libs.json._
// import play.api.libs.functional.syntax._
import org.scalatestplus.play.PlaySpec

import scala.util.{Try,Success,Failure}
import play.api.libs.json._
import base.ProcessJson


class ValueStanzaSpec extends PlaySpec with ProcessJson {

  val validValueStanzaJson: JsValue = Json.parse(
    """{
      |  "type": "ValueStanza",
      |  "values": [
      |    {
      |      "type": "scalar",
      |      "label": "PageName",
      |      "value": "Telling HMRC about extra income"
      |    },
      |    {
      |      "type": "scalar",
      |      "label": "PageUrl",
      |      "value": "/rent/less-than-1000/do-you-want-to-use-the-rent-a-room-scheme"
      |    }
      |  ],
      |  "next": ["40"],
      |  "stack": false
      |}
    """.stripMargin
  )

  "ValueStanza" must {

    "deserialise from json" in {

      val stanza: ValueStanza = validValueStanzaJson.as[ValueStanza]

      stanza.stack mustBe false
      stanza.next mustBe Seq("40")
      stanza.values.length mustBe 2
      stanza.values(0) mustBe Value(Scalar, "PageName", "Telling HMRC about extra income")
      stanza.values(1) mustBe Value(Scalar, "PageUrl", "/rent/less-than-1000/do-you-want-to-use-the-rent-a-room-scheme")
    }
  }
}
