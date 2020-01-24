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
import play.api.libs.json._
import base.ProcessJson

class PhrasesSpec extends PlaySpec with ProcessJson {

  val p1 = "Ask the customer if they have a tea bag"
  val p1w = "Welsh, Ask the customer if they have a tea bag"
  val p2 = "Do you have a tea bag?"
  val p2w = "Welsh, Do you have a tea bag?"
  val p3 = "Yes - they do have a tea bag"
  val p3w = "Welsh, Yes - they do have a tea bag"

  val phrases: Phrases = Json.parse(s"""[ ["$p1", "$p1w" ], ["$p2", "$p2w"], ["$p3", "$p3w"] ]""").as[Phrases]

  "Phrases section" must {

    "deserialise from phrases section json" in {

      phrases mustBe Phrases(Seq(Phrase(Seq(p1,p1w)), Phrase(Seq(p2, p2w)), Phrase(Seq(p3, p3w))))
    }

    "deserialise from phrases section json where lang text is accessible" in {

      val welsh = 1
      val thirdElementIndex = 2
      phrases.elems(thirdElementIndex).langs(welsh) mustBe p3w
    }

  }

}
