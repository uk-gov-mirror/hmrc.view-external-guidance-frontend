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

class LabelSpec extends BaseSpec with ProcessJson {

  val label = """{"name":"BLAH","type":"Value"}"""
  val labelWithValue = """{"name":"BLAH","type":"Value","value":"39.99"}"""

  val displayLabel = """{"name":"BLAH","type":"Display"}"""
  val displayLabelWithValues = """{"name":"BLAH","english":"Hello","type":"Display","welsh":"Welsh, Hello"}"""

  "ValueLabel" must {
    "deserialise " in {
      Json.parse(label).as[Label] shouldBe ValueLabel("BLAH")
      Json.parse(labelWithValue).as[Label] shouldBe ValueLabel("BLAH", Some("39.99"))
    }

    "serialise from Label to json" in {
      val valueLabel: Label = ValueLabel("BLAH")
      Json.toJson(valueLabel).toString shouldBe label
      val valueLabelWithValue: Label = ValueLabel("BLAH", Some("39.99"))
      Json.toJson(valueLabelWithValue).toString shouldBe labelWithValue
    }
  }

  "DisplayLabel" must {
    "deserialise " in {
      Json.parse(displayLabel).as[Label] shouldBe DisplayLabel("BLAH")
      Json.parse(displayLabelWithValues).as[Label] shouldBe DisplayLabel("BLAH", Some("Hello"), Some("Welsh, Hello"))
    }

    "serialise from Label to json" in {
      val dLabel: Label = DisplayLabel("BLAH")
      Json.toJson(dLabel).toString shouldBe displayLabel
      val dLabelWithValues: Label = DisplayLabel("BLAH", Some("Hello"), Some("Welsh, Hello"))
      Json.toJson(dLabelWithValues).toString shouldBe displayLabelWithValues
    }

  }

  "Label" must {
    "contruct Value labels when presented with a single value" in {
      Label("Name", Some("value")) shouldBe ValueLabel("Name", Some("value"))

    }
    "contruct Display labels when presented with a two values" in {
      Label("Name", Some("english"), Some("welsh")) shouldBe DisplayLabel("Name", Some("english"), Some("welsh"))
    }
  }

  "LabelCache" must {
    "Allow reference to the current value of a label" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y", Some("4")), "Name" -> ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("X") shouldBe Some("33.5")

      labels.value("Name") shouldBe Some("Coltrane")

    }

    "Return an empty string if label has no assigned value" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y"), "Name" -> ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("Y") shouldBe Some("")
    }

    "Return None if referenced label does not exist" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y"), "Name" -> ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("Z") shouldBe None
    }

    "Allow the current value of the label to be updated" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y", Some("4")), "Name" -> ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("X") shouldBe Some("33.5")

      val updatedLabels = labels.update("Name", "Miles")

      updatedLabels.value("Name") shouldBe Some("Miles")

    }

    "Allow a new label to be added to the cache" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y", Some("4")), "Name" ->ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("X") shouldBe Some("33.5")

      val updatedLabels = labels.update("Location", "Here")

      updatedLabels.value("Location") shouldBe Some("Here")

    }

    "Return a map of new and updated labels on request" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y", Some("4")), "Name" ->ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)
      labels.value("X") shouldBe Some("33.5")
      val labels1 = labels.update("X", "49.5")

      val labels2 = labels1.update("Location", "Here")

      labels2.updatedLabels shouldBe Map("X" ->ValueLabel("X",Some("49.5")), "Location" ->ValueLabel("Location",Some("Here")))

    }

    "Flush updated labels to main store" in {
      val labelsMap = Map("X"->ValueLabel("X", Some("33.5")), "Y"->ValueLabel("Y", Some("4")), "Name" ->ValueLabel("Name", Some("Coltrane")))
      val labels = LabelCache(labelsMap)

      val labels1 = labels.update("X", "49.5")
      val labels2 = labels1.update("Location", "Here")

      labels2.updatedLabels shouldBe Map("X" ->ValueLabel("X",Some("49.5")), "Location" ->ValueLabel("Location",Some("Here")))

      val labels3 = labels2.flush

      labels3.updatedLabels shouldBe Map()

      labels3.value("X") shouldBe Some("49.5")
      labels3.value("Location") shouldBe Some("Here")

    }

  }

}
