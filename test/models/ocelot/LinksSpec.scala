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

class LinksSpec extends PlaySpec {

  val id1 = 0
  val dest1 = "http://www.bbc.co.uk/news"
  val title1 = "BBC News"
  val id2 = 1
  val dest2 = "http://gov.uk"
  val title2 = "GOV"
  val window = false
  val leftbar = false
  val always = false
  val popUp = false

  val linkStr1: String =
    s"""
       |{
       |   "id": ${id1},
       |   "dest": "${dest1}",
       |   "title": "${title1}",
       |   "window": ${window},
       |   "leftbar": ${leftbar},
       |   "always": ${always},
       |   "popup": ${popUp}
       |}
    """.stripMargin

  val linkStr2: String =
    s"""
       |{
       |   "id": ${id2},
       |   "dest": "${dest2}",
       |   "title": "${title2}",
       |   "window": ${window},
       |   "leftbar": ${leftbar},
       |   "always": ${always},
       |   "popup": ${popUp}
       |}
    """.stripMargin

  val link1Json = Json.parse(linkStr1).as[JsObject]
  val link2Json = Json.parse(linkStr2).as[JsObject]

  val linksStr: String = s"""[${linkStr1}, ${linkStr2}]"""

  val link1: Link = Link( id1, dest1, title1, window, leftbar, always, popUp)
  val link2: Link = Link( id2, dest2, title2, window, leftbar, always, popUp)

  "Link" must {
    "deserialise from Link json" in {

      link1Json.as[Link] mustBe link1

      link2Json.as[Link] mustBe link2
    }

    link1Json.keys.foreach { attributeName =>
      s"throw exception when json is missing attribute $attributeName" in {
        val invalidJson = link1Json - attributeName
        invalidJson.validate[Link] match {
          case JsSuccess(_, _) => fail(s"Link object incorrectly created when attribute $attributeName missing")
          case JsError(_) => succeed
        }
      }
    }

  }

  "Links section" must {

    "deserialise from links section json" in {

      Json.parse(linksStr).as[Links] mustBe Links(Vector(link1, link2))

    }

  }

}
