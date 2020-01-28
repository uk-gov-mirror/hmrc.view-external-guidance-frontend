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

package models

import base.{BaseSpec, ProcessJson}
import play.api.libs.json._

import models.ocelot._
import services.PageBuilder

class PageSpec extends BaseSpec with ProcessJson {

  "Pages" must {

    "be not buildable from non-existent key" in {


      val process:Process = prototypeJson.as[Process]

      PageBuilder.buildPage("unknown", process) mustBe None
    }
  }

  "Sequence of connected pages" must {

    "not be extractable from a Process using an invalid start key" in {

      val process:Process = prototypeJson.as[Process]

      PageBuilder.pages(process, "unknown") mustBe Nil
    }

    "be extractable from a Process using key 'start''" in {

      val process:Process = prototypeJson.as[Process]

      val pages = PageBuilder.pages(process, "start")

      pages mustNot be (Nil)

      pages.length mustBe 27
    }

    "consist of one page when only page exists" in {
      val process:Process = validOnePageJson.as[Process]

      val pages = PageBuilder.pages(process, "1")

      pages mustNot be (Nil)

      pages.length mustBe 1

    }
  }
}
