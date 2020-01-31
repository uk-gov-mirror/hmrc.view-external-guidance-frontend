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

import base.BaseSpec
import models.ocelot.stanzas._
import models.ocelot.{Page, _}
import play.api.libs.json._
import utils.StanzaHelper
import play.api.i18n.Lang
import play.api.{Configuration, Environment, _}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import config.AppConfig
import scala.collection.immutable.ListMap

class PageBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  implicit val lang: Lang = Lang("en")

  private val env = Environment.simple()
  private val configuration = Configuration.load(env)

  private val serviceConfig = new ServicesConfig(configuration, new RunMode(configuration, Mode.Dev))
  private val appConfig = new AppConfig(configuration, serviceConfig)

  val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]
  val supportedLanguagesInOrder: List[Lang] = appConfig.inOrderLanguageMap.values.toList

  case object DummyStanza extends Stanza {
    override val next: Seq[String] = Seq("1")
  }

  "PageBuilder error handling" must {

    val flow = Map(
      "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      "1" -> InstructionStanza(0, Seq("2"), None, false),
      "2" -> DummyStanza
    )

    "detect UnknownStanza error" in {

      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text"))), Vector[Link]())

      PageBuilder(supportedLanguagesInOrder).buildPage("start", process) match {
        case Left(UnknownStanza(DummyStanza)) => succeed
        case Left(stanza) => fail(s"Unknown stanza not detected, found $stanza")
        case err => fail(s"Unknown stanza not detected $err")
      }

    }

    "detect NoSuchPage error" in {
      val process = Process(metaSection, flow, Vector[Phrase](), Vector[Link]())

      PageBuilder(supportedLanguagesInOrder).buildPage("4", process) match {
        case Left(NoSuchPage("4")) => succeed
        case _ => fail("Unknown stanza not detected")
      }
    }

    "detect MissingPageUrlValueStanza error" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder(supportedLanguagesInOrder).pages(process) match {
        case Left(MissingPageUrlValueStanza("4")) => succeed
        case Left(err) => fail(s"Missing ValueStanza containing PageUrl value not detected, failed with $err")
        case _ => fail(s"Missing ValueStanza containing PageUrl value not detected")
      }
    }

    "detect MissingPageUrlWithinValueStanza error" in {
      val flow = Map(
        "start" -> ValueStanza(List(Value(Scalar, "SomeValue", "Blah")), Seq("1"), false),
        "1" -> InstructionStanza(0, Seq("2"), None, false),
        "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
        "4" -> InstructionStanza(0, Seq("end"), None, false),
        "5" -> InstructionStanza(0, Seq("end"), None, false),
        "end" -> EndStanza
      )
      val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                              Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                              Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                              Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

      PageBuilder(supportedLanguagesInOrder).pages(process) match {
        case Left(MissingPageUrlValueStanza("start")) => succeed
        case Left(err) => fail(s"Missing PageUrl value within initial ValueStanza not found with error $err")
        case Right(_) => fail(s"Missing PageUrl value within initial ValueStanza missed")
      }
    }

  }

  "PageBuilder" must {

    "be not buildable from non-existent key" in {


      val process: Process = prototypeJson.as[Process]

      PageBuilder(supportedLanguagesInOrder).buildPage("unknown", process) match {
        case Right(_) => fail("Invalid key should not return a page")
        case Left(err) => succeed
      }
    }

    "Sequence of connected pages" must {

      "not be extractable from a Process using an invalid start key" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder(supportedLanguagesInOrder).pages(process, "unknown") match {
          case Right(_) => fail("""Should fail with NoSuchPage("unknown")""")
          case Left(err) if err == NoSuchPage("unknown") => succeed
          case Left(wrongErr) => fail("""Should fail with NoSuchPage("unknown")""")
        }
      }

      "be extractable from a Process using key 'start''" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder(supportedLanguagesInOrder).pages(process) match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 28

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "correctly identify the pages in a Process accounting fro every stanza" in {

        val process: Process = prototypeJson.as[Process]

        PageBuilder(supportedLanguagesInOrder).pages( process ) match {
          case Right(pages) => {

            testPagesInPrototypeJson( pages )

            checkAllStanzasAllocatedToPages( process.flow, pages )
          }
          case Left(err) => fail( s"Flow error $err" )
        }


      }

      "consist of one page when only page exists" in {
        val process: Process = validOnePageJson.as[Process]

        PageBuilder(supportedLanguagesInOrder).pages(process, "1") match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "confirm one page elements" in {

        val process: Process = Process(meta, onePage, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                                     Phrase(Vector("Some Text1","Welsh, Some Text1"))), Vector[Link]())

        PageBuilder(supportedLanguagesInOrder).pages(process) match {
          case Right(pages) =>
            pages mustNot be(Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }
      }
    }

    "When processing a simple question page" must {

      val process: Process = Process(meta, simpleQuestionPageAlt, Vector(), Vector())

      PageBuilder(supportedLanguagesInOrder).pages(process) match {

        case Right(pages) =>

          "Determine the correct number of pages to be displayed" in {

            pages mustNot be(Nil)

            pages.length mustBe 3
          }

          val indexedSeqOfPages = pages.toIndexedSeq

          //Test contents of individual pages
          testSqpQp(indexedSeqOfPages(0))

          testSqpFap(indexedSeqOfPages(1))

          testSqpSap(indexedSeqOfPages(2))

        case Left(err) => fail(s"Flow error $err")
      }

    }

  }

  "When processing a 2 page flow seperated by a ValueStanza" must {
    val process: Process = Process(meta, twoPagesSeperatedByValueStanza, Vector(Phrase(Vector("Some Text","Welsh, Some Text")),
                                                                                Phrase(Vector("Some Text1","Welsh, Some Text1"))), Vector[Link]())
    "result in 2 pages" in {
      PageBuilder(supportedLanguagesInOrder).pages(process) match {
        case Right(pages) if pages.length == 2 => succeed
        case Right(pages) => fail(s"Page count is incorrect, found ${pages.length} pages")
        case Left(err) => fail(s"FAIL ${err.toString}")
      }
    }
  }

  def testPagesInPrototypeJson( pages: Seq[Page] ) : Unit = {

    val expectedPageIds: List[String] = List( "start", "26", "36", "37", "39", "46", "53", "60",
                                              "70", "77", "120", "80", "83", "90", "97", "102",
                                              "109", "113", "121", "124", "127", "131", "159",
                                              "138", "143", "151", "157", "158" )

    val indexedPages: IndexedSeq[Page] = pages.toIndexedSeq

    expectedPageIds.zipWithIndex.foreach{
      case( id, index ) => indexedPages( index ).id mustBe id
    }
  }

  def checkAllStanzasAllocatedToPages(flowStanzas: Map[String, Stanza], pages:Seq[Page]): Unit = {

    for( ( key, stanza ) <- flowStanzas ) {

      pages.exists( _.stanzas.contains( key ) ) mustBe true

    }

  }

  /**
   * Test question page in simple question page test
   *
   * @param firstPage
   */
  def testSqpQp(firstPage: Page): Unit = {

    "Define the question page correctly" in {

      firstPage.id mustBe "start"
      firstPage.stanzas.size mustBe 4

      firstPage.stanzas.get("start") mustBe Some(sqpQpValueStanza)
      firstPage.stanzas.get("1") mustBe Some(sqpQpInstruction)
      firstPage.stanzas.get("2") mustBe Some(sqpQpCallout)
      firstPage.stanzas.get("3") mustBe Some(sqpQpQuestion)

      firstPage.next mustBe Seq("4", "6")
    }

  }

  /**
   * Test first answer page in simple question page test
   *
   * @param secondPage
   */
  def testSqpFap(secondPage: Page): Unit = {

    "Define the first answer page correctly" in {

      secondPage.id mustBe "4"
      secondPage.stanzas.size mustBe 3

      secondPage.stanzas.get("4") mustBe Some(sqpFapValueStanza)
      secondPage.stanzas.get("5") mustBe Some(sqpFapInstruction)
      secondPage.stanzas.get("end") mustBe Some(EndStanza)

      secondPage.next mustBe Nil
    }

  }

  /**
   * Test second answer page in simple question page
   *
   * @param thirdPage
   */
  def testSqpSap(thirdPage: Page): Unit = {

    "Define the second answer page correctly" in {

      thirdPage.id mustBe "6"
      thirdPage.stanzas.size mustBe 4

      thirdPage.stanzas.get("6") mustBe Some(sqpSapValueStanza)
      thirdPage.stanzas.get("7") mustBe Some(sqpSapInstruction)
      thirdPage.stanzas.get("8") mustBe Some(sqpSapCallout)
      thirdPage.stanzas.get("end") mustBe Some(EndStanza)

      thirdPage.next mustBe Nil
    }

  }
}
