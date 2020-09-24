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
import repositories.ProcessContext
import uk.gov.hmrc.http.HeaderCarrier
import models.ui.FormData
import scala.concurrent.{ExecutionContext, Future}

trait MockGuidanceService extends MockFactory {

  val mockGuidanceService: GuidanceService = mock[GuidanceService]

  object MockGuidanceService {

    def retrieveAndCacheScratch(uuid: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] = {
      (mockGuidanceService
        .retrieveAndCacheScratch(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(uuid, *, *, *)
    }

    def retrieveAndCachePublished(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String,String)]]] = {
      (mockGuidanceService
        .retrieveAndCachePublished(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)
    }

    def retrieveAndCacheApproval(processId: String, sessionRepoId: String): CallHandler[Future[RequestOutcome[(String, String)]]] = {
      (mockGuidanceService
        .retrieveAndCacheApproval(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *, *)
    }

    def getProcessContext(sessionId: String, pageUrl: String): CallHandler[Future[RequestOutcome[ProcessContext]]] = {
      (mockGuidanceService
        .getProcessContext(_: String, _: String))
        .expects(sessionId, pageUrl)
    }

    def getProcessContext(sessionId: String): CallHandler[Future[RequestOutcome[ProcessContext]]] = {
      (mockGuidanceService
        .getProcessContext(_: String))
        .expects(sessionId)
    }

    def getPageContext(processId: String, url: String, sessionId: String, formData: Option[FormData]): CallHandler[Future[RequestOutcome[PageContext]]] = {
      (mockGuidanceService
        .getPageContext(_: String, _: String, _: String, _: Option[FormData])(_: ExecutionContext))
        .expects(processId, url, sessionId, formData, *)
    }

    def saveAnswerToQuestion(docId: String, url: String, answer: String): CallHandler[Future[RequestOutcome[Unit]]] = {
      (mockGuidanceService
        .saveAnswerToQuestion(_: String, _: String, _: String))
        .expects(docId, url, answer)
    }

  }

}
