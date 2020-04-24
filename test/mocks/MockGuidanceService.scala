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

package mocks

import models.ui.PageContext
import models.RequestOutcome
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.GuidanceService
import uk.gov.hmrc.http.HeaderCarrier
import models.ui.FormData
import scala.concurrent.{ExecutionContext, Future}

trait MockGuidanceService extends MockFactory {

  val mockGuidanceService: GuidanceService = mock[GuidanceService]

  object MockGuidanceService {

    def scratchProcess(uuid: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[String]]] = {
      (mockGuidanceService
        .scratchProcess(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(uuid, *, *, *)
    }

    def getStartPageUrl(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[String]]] = {
      (mockGuidanceService
        .getStartPageUrl(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)
    }

    def publishedProcess(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[String]]] = {
      (mockGuidanceService
        .publishedProcess(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)
    }

    def getPageContext(url: String, processId: String, formData: Option[FormData]): CallHandler[Future[RequestOutcome[PageContext]]] = {
      (mockGuidanceService
        .getPageContext(_: String, _: String, _: Option[FormData])(_: ExecutionContext))
        .expects(url, processId, formData, *)
    }

  }

}
