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

package core.models.ocelot.stanzas

import base.{BaseSpec, TestConstants}
import core.models.ocelot.{LabelCache, Phrase}
import play.api.libs.json._


class ChooserStanzaSpec extends BaseSpec with TestConstants {

  val twoStr: String = "2"
  val fourStr: String = "4"
  val fiveStr: String = "5"
  val sevenStr: String = "7"
  val eightStr: String = "8"

  val stanzaType: String = "ChooserStanza"

  val stack: Boolean = false

  val chooserStanzaJsonInput: String =
    s"""|{
       | "text": $zero,
       | "next": ["$twoStr", "$fourStr"],
       | "source": "$fiveStr",
       | "min": $one,
       | "max": $two,
       | "stack": $stack
       |}""".stripMargin


  val validChooserStanzaJson: JsObject = Json.parse(chooserStanzaJsonInput).as[JsObject]

  "Chooser stanza" must {

    "Evaluate valid input and return a next stanza and updated Labels" in {
      val labels = LabelCache()
      val stanza: Chooser = Chooser(Phrase("text",""), Seq("2", "4"), "1,2,3,4,5", 1, 2, Some(stanzaType), stack = true)
      val (nxt, _) = stanza.eval("1,2", labels)

      nxt shouldBe Some("2")
    }

    "Evaluate invalid input (selection not in source) and return None and original Labels" in {
      val labels = LabelCache()
      val stanza: Chooser = Chooser(Phrase("text",""), Seq("2", "4"), "1,2,3,4,5", 1, 2, None, stack = true)
      val (nxt, _) = stanza.eval("6", labels)

      nxt shouldBe None
    }

    "Evaluate invalid input (no selections) and return None and original Labels" in {
      val labels = LabelCache()
      val stanza: Chooser = Chooser(Phrase("text",""), Seq("2", "4"), "1,2,3,4,5", 2, 3, Some(stanzaType), stack = true)
      val (nxt, _) = stanza.eval("", labels)

      nxt shouldBe None
    }

    "Evaluate invalid input (too few selections) and return None and original Labels" in {
      val labels = LabelCache()
      val stanza: Chooser = Chooser(Phrase("text",""), Seq("2", "4"), "1,2,3,4,5", 2, 3, Some(stanzaType), stack = true)
      val (nxt, _) = stanza.eval("2", labels)

      nxt shouldBe None
    }

    "Evaluate invalid input (too many selections) and return None and original Labels" in {
      val labels = LabelCache()
      val stanza: Chooser = Chooser(Phrase("text",""), Seq("2", "4"), "1,2,3,4,5", 1, 2, Some(stanzaType), stack = true)
      val (nxt, _) = stanza.eval("1,2,3", labels)

      nxt shouldBe None
    }

    "builder Chooser from ChooserStanza" in {
      val stanza: ChooserStanza = ChooserStanza(0, Seq("2", "4"), "5", 1, 2, Some(stanzaType), stack = true)
      val phrase = Phrase("text", "")

      Chooser.apply(stanza, phrase) shouldBe Chooser(phrase, Seq("2", "4"), "5", 1, 2, Some(stanzaType), stack = true)
    }

    "serialise to json" in {
      val stanza: ChooserStanza = ChooserStanza(0, Seq("2", "4"), "5", 1, 2, Some(stanzaType), stack = true)
      val expectedJson: String = """{"text":0,"next":["2","4"],"source":"5","min":1,"max":2,"label":"ChooserStanza","stack":true}"""
      Json.toJson(stanza).toString shouldBe expectedJson
      Json.fromJson[ChooserStanza](Json.parse(expectedJson)).get shouldBe stanza
    }

    "serialise json to a Stanza reference" in {
      val stanza: Stanza = ChooserStanza(0, Vector("2", "4"), "5", 1, 2, Some(stanzaType), stack = true)
      val expectedJson: String = """{"next":["2","4"],"stack":true,"min":1,"max":2,"text":0,"source":"5","label":"ChooserStanza","type":"ChooserStanza"}"""
      Json.toJson(stanza).toString shouldBe expectedJson
      Json.fromJson[ChooserStanza](Json.parse(expectedJson)).get shouldBe stanza
    }

    /** Test for missing properties in Json object */
    missingJsObjectAttrTests[ChooserStanza](validChooserStanzaJson, List("type"))

    /** Test for properties of the wrong type in json object representing question stanza */
    incorrectPropertyTypeJsObjectAttrTests[ChooserStanza](validChooserStanzaJson, List("type"))

  }

}
