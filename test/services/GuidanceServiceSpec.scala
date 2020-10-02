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
import models.ocelot.{Page, KeyedStanza, Process, ProcessJson, LabelCache}
import models.ui
import uk.gov.hmrc.http.HeaderCarrier
import repositories.ProcessContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import models.errors.BadRequestError
import models.ui.PageContext

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockPageRenderer with MockUIBuilder with ProcessJson {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    private def pageWithUrl(id: String, url: String) = Page(id, url, Seq(KeyedStanza("1", EndStanza)), Seq())

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

    val processId = "oct90001"
    val processCode = "CupOfTea"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"
    val labels = LabelCache()

    lazy val target = new GuidanceService(MockAppConfig, mockGuidanceConnector, mockSessionRepository, mockPageBuilder, mockPageRenderer, mockUIBuilder)
  }

  "Calling getPageContext with a valid URL" should {

    "retrieve a page for the process" in new Test {

      override val processCode = "cup-of-tea"

      MockSessionRepository
        .get(sessionRepoId, s"$processCode$lastPageUrl")
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), None))))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      MockPageRenderer
        .renderPage(pages(2), labels)
        .returns((pages(2).stanzas, labels))

      MockUIBuilder
        .fromStanzas(lastPageUrl, pages.last.stanzas, None)
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
        .returns(Future.successful(Right(ProcessContext(fullProcess, Map(lastPageUrl -> "answer"), Map(), None))))

      MockPageBuilder
        .pages(fullProcess)
        .returns(Right(pages))

      MockPageRenderer
        .renderPage(pages(2), labels)
        .returns((pages(2).stanzas, labels))

      MockUIBuilder
        .fromStanzas(lastPageUrl, pages.last.stanzas, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(processCode, lastPageUrl, sessionRepoId)

      whenReady(result) { pageContext =>
        pageContext match {
          case Right(PageContext(_, _, _, _, _, _, _, Some(answer))) => succeed
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
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), None))))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

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
        .set(uuid, processWithUpdatedId, Map())
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
        .set(sessionRepoId, processWithProcessCode, Map())
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
        .set(sessionRepoId, processWithProcessCode, Map())
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

  "Calling saveAnswerToQuestion" should {

    "store the answer in the local repo" in new Test {

      MockSessionRepository
        .saveAnswerToQuestion(sessionRepoId, firstPageUrl, lastPageUrl)
        .returns(Future.successful(Right(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.saveAnswerToQuestion(sessionRepoId, firstPageUrl, lastPageUrl)

      whenReady(result) { ret =>
        ret shouldBe Right({})
      }
    }
  }

  "Calling getProcessContext(key: String)" should {

    "successfully retrieve a process context when the session data contains a single process" in new Test {

      val expectedProcessContext: ProcessContext = ProcessContext(process, Map(), Map(), None)

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

}
