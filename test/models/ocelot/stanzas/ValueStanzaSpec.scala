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

  val stanzaType = "ValueStanza"
  val valueType = "scalar"
  val pageNameLabel = "PageName"
  val pageName = "Telling HMRC about extra income"
  val pageUrlLabel = "PageUrl"
  val pageUrl = "/rent/less-than-1000/do-you-want-to-use-the-rent-a-room-scheme"
  val next = "40"
  val stack = "false"

  val validValueStanzaJson: JsValue = Json.parse(
    s"""{
      |  "type": "${stanzaType}",
      |  "values": [
      |    {
      |      "type": "${valueType}",
      |      "label": "${pageNameLabel}",
      |      "value": "${pageName}"
      |    },
      |    {
      |      "type": "${valueType}",
      |      "label": "${pageUrlLabel}",
      |      "value": "${pageUrl}"
      |    }
      |  ],
      |  "next": ["${next}"],
      |  "stack": ${stack}
      |}
    """.stripMargin
  )

  "ValueStanza" must {

    "deserialise from json" in {

      val stanza: ValueStanza = validValueStanzaJson.as[ValueStanza]

      stanza.stack mustBe false
      stanza.next.length mustBe 1
      stanza.next(0) mustBe next
      stanza.values.length mustBe 2
      stanza.values(0) mustBe Value(Scalar, pageNameLabel, pageName)
      stanza.values(1) mustBe Value(Scalar, pageUrlLabel, pageUrl)
    }
  }
}
