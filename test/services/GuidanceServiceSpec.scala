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
import mocks.{MockGuidanceConnector, MockSessionRepository}
import models.ocelot.{Process, ProcessJson}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class GuidanceServiceSpec extends BaseSpec {

  private trait Test extends MockGuidanceConnector with MockSessionRepository with ProcessJson {
    implicit val headerCarrier: HeaderCarrier = HeaderCarrier()
    val process: Process = prototypeJson.as[Process]

    val pageBuilder: PageBuilder = new PageBuilder()
    val uiBuilder: UIBuilder = new UIBuilder()
    lazy val target = new GuidanceService(mockGuidanceConnector, mockSessionRepository, pageBuilder, uiBuilder)
  }

  "Calling getPage with a valid URL" should {

    "retrieve a page for the process" in new Test {
      val processId = "ext90001"
      val url = "/"

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Some(process)))

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
      val processId = "ext90001"
      val url = "scooby"

      MockSessionRepository
        .get(processId)
        .returns(Future.successful(Some(process)))

      private val result = target.getPage(url, processId)

      whenReady(result) { _ mustBe None }
    }
  }

  "Calling getStartPageUrl" should {

    "retrieve the url of the start page for the process" in new Test {
      val processId = "ext90001"

      MockGuidanceConnector
        .getProcess(processId)
        .returns(Future.successful(Some(process)))
        
      MockSessionRepository
        .set(processId, process)
        .returns(Future.successful(Some(())))

      private val result = target.getStartPageUrl(processId, processId)

      whenReady(result) { url =>
        url mustBe Some("/")
      }
    }
  }

}
