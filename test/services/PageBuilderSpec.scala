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
import models.Page
import models.ocelot._
import models.ocelot.stanzas.EndStanza
import utils.StanzaHelper

class PageBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  val meta: Meta = Json.parse( prototypeMetaSection ).as[Meta]

  "PageBuilder" must {

    "be not buildable from non-existent key" in {


      val process:Process = prototypeJson.as[Process]

      PageBuilder.buildPage("unknown", process) mustBe None

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

      "confirm one page elements" in {

        val process:Process = Process( meta, onePage, Vector(), Vector())

        val pages = PageBuilder.pages(process, "start")

        pages mustNot be (Nil)

        pages.length mustBe 1
      }

      "When processing a simple question page" must {

        val process: Process = Process( meta, simpleQuestionPage, Vector(), Vector() )

        val pages: Seq[Page] = PageBuilder.pages(  process, "start" )

        "Determine the correct number of pages to be displayed" in {

          pages mustNot be(Nil)

          pages.length mustBe 3
        }

        val indexedSeqOfPages = pages.toIndexedSeq

        "Define the question page correctly" in {

          val firstPage: Page = indexedSeqOfPages(0)

          firstPage.id mustBe "start"
          firstPage.stanzas.size mustBe 4

          firstPage.stanzas.get( "start" ) mustBe Some( sqpQpValueStanza )
          firstPage.stanzas.get( "1" ) mustBe Some( sqpQpInstructionStanza )
          firstPage.stanzas.get( "2" ) mustBe Some( sqpQpCalloutStanza )
          firstPage.stanzas.get( "3" ) mustBe Some( sqpQpQuestionStanza )

          firstPage.next mustBe Seq( "4", "6" )
        }

        "Define the first answer page correctly" in {

          val secondPage: Page = indexedSeqOfPages(1)

          secondPage.id mustBe "4"
          secondPage.stanzas.size mustBe 3

          secondPage.stanzas.get( "4" ) mustBe Some( sqpFapValueStanza )
          secondPage.stanzas.get( "5" ) mustBe Some( sqpFapInstructionStanza )
          secondPage.stanzas.get( "end" ) mustBe Some( EndStanza )

          secondPage.next mustBe (Nil)
        }

        "Define the second answer page correctly" in {

          val thirdPage: Page = indexedSeqOfPages(2)

          thirdPage.id mustBe "6"
          thirdPage.stanzas.size mustBe 4

          thirdPage.stanzas.get( "6" ) mustBe Some( sqpSapValueStanza )
          thirdPage.stanzas.get( "7" ) mustBe Some( sqpSapInstructionStanza )
          thirdPage.stanzas.get( "8" ) mustBe Some( sqpSapCalloutStanza )
          thirdPage.stanzas.get( "end" ) mustBe Some( EndStanza )

          thirdPage.next mustBe (Nil)
        }

      }

    }

  }

}
