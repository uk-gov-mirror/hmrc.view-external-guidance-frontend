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
import models.ocelot.stanzas._
import models.ocelot.{Page, Process, ProcessJson}
import models.ui
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with MockPageBuilder with MockUIBuilder with ProcessJson {

    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    implicit val stanzaIdToUrl: Map[String, String] = Map[String, String]()

    private def pageWithUrl(id: String, url: String) = Page(id, url, Seq(EndStanza), Seq())

    val process: Process = validOnePageJson.as[Process]

    val firstPageUrl = "/first-page"
    val firstUiPage: ui.Page = ui.Page(firstPageUrl, Seq())

    val lastPageUrl = "/last-page"
    val lastUiPage: ui.Page = ui.Page(lastPageUrl, Seq())

    val pages: Seq[Page] = Seq(
      pageWithUrl("start", firstPageUrl),
      pageWithUrl("1", "/page-1"),
      pageWithUrl("2", lastPageUrl)
    )

    val processId = "ext90001"
    val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    val sessionRepoId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"

    lazy val target = new GuidanceService(mockGuidanceConnector, mockSessionRepository, mockPageBuilder, mockUIBuilder)
  }

  "Calling getPage with a valid URL" should {

    "retrieve a page for the process" in new Test {

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Some(process)))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      MockUIBuilder
        .fromStanzaPage(pages.last, None)
        .returns(lastUiPage)

      private val result = target.getPageContext(lastPageUrl, processId)

      whenReady(result) { pageContext =>
        pageContext.fold(fail("no PageContext found")) {
          _.page.urlPath mustBe lastPageUrl
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

      private val result = target.getPageContext(url, processId)

      whenReady(result) {
        _ mustBe None
      }
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
        url mustBe Some(firstPageUrl)
      }
    }
  }

  "Calling scratchProcess" should {

    "retrieve the url of the start page for the scratch process" in new Test {

      MockGuidanceConnector
        .scratchProcess(uuid)
        .returns(Future.successful(Some(process)))

      MockSessionRepository
        .set(uuid, process)
        .returns(Future.successful(Some(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.scratchProcess(uuid, uuid)

      whenReady(result) { url =>
        url mustBe Some(firstPageUrl)
      }
    }
  }

  "Calling publishedProcess" should {

    "retrieve the url of the start page for the nominated published process" in new Test {

      MockGuidanceConnector
        .publishedProcess(processId)
        .returns(Future.successful(Some(process)))

      MockSessionRepository
        .set(sessionRepoId, process)
        .returns(Future.successful(Some(())))

      MockPageBuilder
        .pages(process)
        .returns(Right(pages))

      private val result = target.publishedProcess(processId, sessionRepoId)

      whenReady(result) { url =>
        url mustBe Some(firstPageUrl)
      }
    }
  }

}
