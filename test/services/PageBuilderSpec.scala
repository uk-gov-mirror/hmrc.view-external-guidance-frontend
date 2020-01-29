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

package services

import play.api.libs.json._
import base.{BaseSpec, ProcessJson}
import models.ocelot._
import utils.StanzaHelper

class PageBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  val meta: Meta = Json.parse( prototypeMetaSection ).as[Meta]

  "PageBuilder" must {

    "be not buildable from non-existent key" in {


      val process:Process = prototypeJson.as[Process]

      PageBuilder.buildPage("unknown", process) match {
        case Right(_) => fail( "Invalid key should not return a page")
        case Left(err) => succeed
      }
    }

    "Sequence of connected pages" must {

      "not be extractable from a Process using an invalid start key" in {

        val process:Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "unknown") match {
          case Right(_) => fail("""Should fail with NoSuchPage("unknown")""")
          case Left(err) if err == NoSuchPage("unknown") => succeed
          case Left(wrongErr) => fail("""Should fail with NoSuchPage("unknown")""")
        }
      }

      "be extractable from a Process using key 'start''" in {

        val process:Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 27

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "consist of one page when only page exists" in {
        val process:Process = validOnePageJson.as[Process]

        PageBuilder.pages(process, "1") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "confirm one page elements" in {

        val process:Process = Process( meta, onePage, Vector(), Vector())

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }
      }

    }

  }

}
