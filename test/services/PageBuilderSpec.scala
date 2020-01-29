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
import models.ocelot.Page
import models.ocelot._
import models.ocelot.stanzas.EndStanza
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

        val process: Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "unknown") match {
          case Right(_) => fail("""Should fail with NoSuchPage("unknown")""")
          case Left(err) if err == NoSuchPage("unknown") => succeed
          case Left(wrongErr) => fail("""Should fail with NoSuchPage("unknown")""")
        }
      }

      "be extractable from a Process using key 'start''" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 27

            // This test requires an update to the parsing of a process
            //testPagesInPrototypeJson( pages )

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "consist of one page when only page exists" in {
        val process: Process = validOnePageJson.as[Process]

        PageBuilder.pages(process, "1") match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "confirm one page elements" in {

        val process: Process = Process(meta, onePage, Vector(), Vector())

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }
      }
    }

    "When processing a simple question page" must {

      val process: Process = Process( meta, simpleQuestionPage, Vector(), Vector() )

      PageBuilder.pages( process ) match {

        case Right(pages) => {

          "Determine the correct number of pages to be displayed" in {

            pages mustNot be(Nil)

            pages.length mustBe 3
          }

          val indexedSeqOfPages = pages.toIndexedSeq

          // Test contents of individual pages
          testSqpQp( indexedSeqOfPages(0) )

          testSqpFap( indexedSeqOfPages(1) )

          testSqpSap( indexedSeqOfPages(2) )
        }
        case Left(err) => fail(s"Flow error $err")
      }

    }

  }

  def testPagesInPrototypeJson( pages: Seq[Page] ) : Unit = {

    val indexedPages: IndexedSeq[Page] = pages.toIndexedSeq

    indexedPages(0).id mustBe "start"
    indexedPages(1).id mustBe "26"
  }

  /**
   * Test question page in simple question page test
   *
   * @param firstPage
   */
  def testSqpQp( firstPage: Page ) : Unit = {

    "Define the question page correctly" in {

      firstPage.id mustBe "start"
      firstPage.stanzas.size mustBe 4

      firstPage.stanzas.get( "start" ) mustBe Some( sqpQpValueStanza )
      firstPage.stanzas.get( "1" ) mustBe Some( sqpQpInstructionStanza )
      firstPage.stanzas.get( "2" ) mustBe Some( sqpQpCalloutStanza )
      firstPage.stanzas.get( "3" ) mustBe Some( sqpQpQuestionStanza )

      firstPage.next mustBe Seq( "4", "6" )
    }

  }

  /**
   * Test first answer page in simple question page test
   *
   * @param secondPage
   */
  def testSqpFap( secondPage: Page ) : Unit = {

    "Define the first answer page correctly" in {

      secondPage.id mustBe "4"
      secondPage.stanzas.size mustBe 3

      secondPage.stanzas.get( "4" ) mustBe Some( sqpFapValueStanza )
      secondPage.stanzas.get( "5" ) mustBe Some( sqpFapInstructionStanza )
      secondPage.stanzas.get( "end" ) mustBe Some( EndStanza )

      secondPage.next mustBe (Nil)
    }

  }

  /**
   * Test second answer page in simple question page
   *
   * @param thirdPage
   */
  def testSqpSap( thirdPage: Page ) : Unit = {

    "Define the second answer page correctly" in {

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
