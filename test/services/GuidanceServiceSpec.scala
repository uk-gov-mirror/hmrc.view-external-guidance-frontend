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
import mocks.{MockAppConfig, MockGuidanceConnector, MockPageBuilder, MockPageRenderer, MockSessionRepository, MockUIBuilder}
import models.errors.{DatabaseError, NotFoundError}
import models.ocelot.stanzas._
import models.ocelot.{Page, KeyedStanza, Process, ProcessJson, LabelCache, Phrase}
import models.ui
import models.PageEvaluationContext
import uk.gov.hmrc.http.HeaderCarrier
import repositories.ProcessContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models.errors.BadRequestError
import models.PageContext

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockPageRenderer with MockUIBuilder with ProcessJson {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    def pageWithUrl(id: String, url: String) = Page(id, url, Seq(KeyedStanza("1", EndStanza)), Seq())

    val process: Process = validOnePageJson.as[Process]
    val processWithProcessCode = validOnePageProcessWithProcessCodeJson.as[Process]
    val fullProcess: Process = prototypeJson.as[Process]

    val firstPageUrl = "/first-page"
    val firstUiPage: ui.Page = ui.Page(firstPageUrl, Seq())

    val lastPageUrl = "/last-page"
    val lastUiPage: ui.Page = ui.Page(lastPageUrl, Seq())

    val pages: Seq[Page] = Seq(
      pageWithUrl(Process.StartStanzaId, firstPageUrl),
      pageWithUrl("1", "/page-1"),
      pageWithUrl("2", lastPageUrl)
    )

    val lastPage = pageWithUrl("2", lastPageUrl)

    val processId = "oct90001"
    val processCode = "CupOfTea"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"
    val labels = LabelCache()

    val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
    val questionStanza = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", questionStanza)
                                      )
    val page = Page("start", "/test-page", stanzas, Seq("4","5"))

    val pec = PageEvaluationContext(
                page,
                processId,
                Map("/first-page" -> "start", "/page-1" -> "1", "/last-page" -> "2"),
                Some("/hello"),
                ui.Text(),
                processId,
                "hello",
                LabelCache(),
                None,
                None
              )

    lazy val target = new GuidanceService(
      MockAppConfig,
      mockGuidanceConnector,
      mockSessionRepository,
      mockPageBuilder,
      mockPageRenderer,
      mockUIBuilder)
  }

  "Calling getPageContext with a valid URL" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(sessionRepoId, s"$processCode$lastPageUrl")
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(lastPageUrl -> "2"), None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None))

      MockSessionRepository
        .saveLabels(sessionRepoId, Seq.empty)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(processCode, lastPageUrl, sessionRepoId)

      whenReady(result) {
        case Right(pc) => pc.page.urlPath shouldBe lastPageUrl
        case Left(err) => fail(s"no PageContext found with error $err")
      }
    }
  }

  "Calling getPageContext against a previously answered Question page url" should {

    "retrieve a PageContext which includes the relevant answer" in new Test {
      override val processId: String = "ext90002"

      override val processCode = "tell-hmrc"

      MockSessionRepository
        .get(sessionRepoId, s"$processCode$lastPageUrl")
        .returns(Future.successful(Right(ProcessContext(fullProcess, Map(lastPageUrl -> "answer"), Map(), Map(lastPageUrl -> "2"), None))))

      MockPageBuilder
        .buildPage("2", fullProcess)
        .returns(Right(lastPage))

      MockPageRenderer
        .renderPage(lastPage, labels)
        .returns((lastPage.stanzas.collect{case s: VisualStanza => s}, labels, None))

      MockSessionRepository
        .saveLabels(sessionRepoId, Seq.empty)
        .returns(Future.successful(Right({})))

      MockUIBuilder
        .buildPage(lastPageUrl, lastPage.stanzas.collect{case s: VisualStanza => s}, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(processCode, lastPageUrl, sessionRepoId)

      whenReady(result) { pageContext =>
        pageContext match {
          case Right(PageContext(_, _, _, _, _, _, _, _, Some(answer))) => succeed
          case Right(wrongContext) => fail(s"Previous answer missing from PageContext, $wrongContext")
          case Left(err) => fail(s"Previous answer missing from PageContext, $err")
        }
      }
    }
  }

  "Calling getPageContext with an invalid URL" should {

    "not retrieve a page from the process" in new Test {

      val url = "/scooby"
      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(processId, s"$processCode$url")
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))

      MockPageBuilder
        .buildPage("2", process)
        .returns(Right(lastPage))

      private val result = target.getPageContext(processCode, url, processId)

      whenReady(result) {
        _ shouldBe Left(BadRequestError)
      }
    }
  }

  "Calling retrieveAndCacheScratch" should {

    "retrieve the url of the start page for the scratch process" in new Test {

      MockGuidanceConnector
        .scratchProcess(uuid)
        .returns(Future.successful(Right(process)))

      val processWithUpdatedId = process.copy(meta = process.meta.copy( id = uuid))

      MockSessionRepository
        .set(uuid, processWithUpdatedId, Map("/first-page" -> "start", "/page-1" -> "1", "/last-page" -> "2"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithUpdatedId)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheScratch(uuid, uuid)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl,"cup-of-tea"))
      }
    }
  }

  "Calling retrieveAndCachePublished" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .set(sessionRepoId, processWithProcessCode, Map("/first-page" -> "start", "/page-1" -> "1", "/last-page" -> "2"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCachePublished(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }
  }

  "Calling retrieveAndCacheApproval" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .approvalProcess(processId)
        .returns(Future.successful(Right(processWithProcessCode)))

      MockSessionRepository
        .set(sessionRepoId, processWithProcessCode, Map("/first-page" -> "start", "/page-1" -> "1", "/last-page" -> "2"))
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(processWithProcessCode)
        .returns(Right(pages))

      private val result = target.retrieveAndCacheApproval(processId, sessionRepoId)

      whenReady(result) { url =>
        url shouldBe Right((firstPageUrl, processCode))
      }
    }
  }

  "Calling getProcessContext(key: String)" should {

    "successfully retrieve a process context when the session data contains a single process" in new Test {

      val expectedProcessContext: ProcessContext = ProcessContext(process, Map(), Map(), Map(), None)

      MockSessionRepository
        .get(sessionRepoId)
        .returns(Future.successful(Right(expectedProcessContext)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { processContext =>
        processContext shouldBe Right(expectedProcessContext)
      }
    }

    "return a not found error if the session data does not exist" in new Test {

      MockSessionRepository
        .get(sessionRepoId)
        .returns(Future.successful(Left(NotFoundError)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(NotFoundError)
      }
    }

    "return a database error if an error occurs retrieving the session data" in new Test {

      MockSessionRepository
        .get(sessionRepoId)
        .returns(Future.successful(Left(DatabaseError)))

      private val result = target.getProcessContext(sessionRepoId)

      whenReady(result) { err =>
        err shouldBe Left(DatabaseError)
      }

    }
  }

  "Calling submitPage" should {
    "Return None if page submission evaluation determines no valid next page" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns((None, LabelCache()))

      MockSessionRepository
        .saveUserAnswerAndLabels(processId,"/test-page", "yes", Seq.empty)
        .returns(Future.successful(Right({})))

      target.submitPage(pec, "/test-page", "yes", "yes").map{
        case Left(err) => fail
        case Right((nxt, lbls)) if nxt.isEmpty => succeed
        case Right(_) => fail
      }
    }

    "Return the id of the page to follow" in new Test {
      MockPageRenderer
        .renderPagePostSubmit(page, LabelCache(), "yes")
        .returns((Some("4"), LabelCache()))

      MockSessionRepository
        .saveUserAnswerAndLabels(processId,"/test-page", "yes", Seq.empty)
        .returns(Future.successful(Right({})))

      target.submitPage(pec, "/test-page", "yes", "yes").map{
        case Left(err) => fail
        case Right((Some("4"), _)) => succeed
        case Right(_) => fail
      }
    }

  }

  "Calling saveLabels" should {
    "Success when labels saved successfully" in new Test {
      MockSessionRepository
        .saveLabels(processId, Seq.empty)
        .returns(Future.successful(Right({})))

      target.saveLabels(processId, LabelCache()).map{
        case Right(x) if x == Unit => succeed
        case Left(_) => fail()
      }
    }

    "An error when labels not saved successfully" in new Test {
      MockSessionRepository
        .saveLabels(processId, Seq.empty)
        .returns(Future.successful(Left(DatabaseError)))

      target.saveLabels(processId, LabelCache()).map{
        case Left(err) if err == DatabaseError => succeed
        case _ => fail()
      }
    }
  }

}
