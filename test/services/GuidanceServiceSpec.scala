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
import mocks.{MockGuidanceConnector, MockPageBuilder, MockSessionRepository, MockUIBuilder}
import models.ocelot.{Link, Meta, Page, Phrase, Process, ProcessJson}
import models.ocelot.stanzas._
import uk.gov.hmrc.http.HeaderCarrier
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockUIBuilder with ProcessJson {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    // Define simple Ocelot process
    private val meta: Meta = Json.parse(prototypeMetaSection).as[Meta]

    private val page1Id: String = "start"
    private val page2Id: String = "4"
    private val page3Id: String = "6"

    private val page1Url: String = "/"
    private val page2Url: String = "/yes"
    private val page3Url: String = "/no"

    private val four: Int = 4
    private val five: Int = 5
    private val six: Int = 6

    private val page1ValueStanza: ValueStanza = ValueStanza(List(Value(Scalar, "PageUrl", page1Url)), Seq("1"), stack = false)
    private val page2ValueStanza: ValueStanza = ValueStanza(List(Value(Scalar, "PageUrl", page2Url)), Seq("5"), stack = false)
    private val page3ValueStanza: ValueStanza = ValueStanza(List(Value(Scalar, "PageUrl", page3Url)), Seq("10"), stack = false)

    private val flow = Map(
      page1Id -> page1ValueStanza,
      "1" -> InstructionStanza(0, Seq("2"), None, stack = false),
      "2" -> QuestionStanza(1, Seq(2, 3), Seq(page2Id, page3Id), stack = false),
      page2Id -> page2ValueStanza,
      "5" -> InstructionStanza(four, Seq("end"), Some(0), stack = false),
      page3Id -> page3ValueStanza,
      "10" -> InstructionStanza(five, Seq("11"), None, stack = false),
      "11" -> InstructionStanza(six, Seq("end"), None, stack = false),
      "end" -> EndStanza
    )

    private val phrases = Vector[Phrase](
      Phrase(Vector("Hello", "Helo")),
      Phrase(Vector("Would you like a cup of tea?", "Hoffech chi gael paned?")),
      Phrase(Vector("Yes please", "os gwelwch yn dda")),
      Phrase(Vector("No thanks", "Dim Diolch")),
      Phrase(Vector("Ok coming up", "Iawn yn dod i fyny")),
      Phrase(Vector("Alright", "Iawn")),
      Phrase(Vector("Perhaps you would like something later", "Efallai yr hoffech gael rhywbeth yn nes ymlaen"))
    )

    private val links = Vector(Link(0, "http://food.com,/tea", "Tea", window = false))

    val process: Process = Process(meta, flow, phrases, links)

    // Define pages created by PageBuilder
    val page1StanzaSeq = Seq(
      page1ValueStanza,
      Instruction(phrases(0), Seq("2"), None, stack = false),
      Question(phrases(1), Seq(phrases(2), phrases(3)), Seq(page2Id, page3Id), stack = false)
    )

    val page1: Page = Page(page1Id, page1Url, page1StanzaSeq, Seq(page2Id, page3Id), Seq.empty)

    val page2StanzaSeq = Seq(
      page2ValueStanza,
      Instruction(phrases(four), Seq("end"), Some(links(0)), stack = false),
      EndStanza
    )

    val page2: Page = Page(page2Id, page2Url, page2StanzaSeq, Seq.empty, Seq.empty)

    val page3StanzaSeq = Seq(
      page3ValueStanza,
      Instruction(phrases(five), Seq("11"), None, stack = false),
      Instruction(phrases(six), Seq("end"), None, stack = false),
      EndStanza
    )

    val page3: Page = Page(page3Id, page3Url, page3StanzaSeq, Seq.empty, Seq.empty)

    val pages: Seq[Page] = Seq(page1, page2, page3)

    // Define first page from UIBuilder
    val uiPage: models.ui.Page = models.ui.Page(page1Url, Seq.empty)

    val pageBuilder: PageBuilder = new PageBuilder()
    val uiBuilder: UIBuilder = new UIBuilder()

    val processId = "ext90001"

    lazy val target = new GuidanceService(mockGuidanceConnector, mockSessionRepository, mockPageBuilder, mockUIBuilder)
  }

  "Calling getPage with a valid URL" should {

    "retrieve a page for the process" in new Test {

      val url = "/"

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Some(process)))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      MockUIBuilder
        .fromStanzaPage(pages.head)
        .returns(uiPage)

      private val result = target.getPage(url, processId)

      whenReady(result) { page =>
        page.fold(fail("no page found")) {
          _.urlPath mustBe url
        }
      }
    }
  }

  "Calling getPage with an invalid URL" should {

    "not retrieve a page from the process" in new Test {

      val url = "scooby"

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Some(process)))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.getPage(url, processId)

      whenReady(result) { _ mustBe None }
    }
  }

  "Calling getStartPageUrl" should {

    "retrieve the url of the start page for the process" in new Test {

      MockGuidanceConnector
        .getProcess(processId)
        .returns(Future.successful(Some(process)))

      MockSessionRepository
        .set(processId, process)
        .returns(Future.successful(Some(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.getStartPageUrl(processId, processId)

      whenReady(result) { url =>
        url mustBe Some("/")
      }
    }
  }

}
