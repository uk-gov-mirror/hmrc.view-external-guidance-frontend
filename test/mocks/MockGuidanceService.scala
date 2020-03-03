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

import models.ui.Page
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import services.GuidanceService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

trait MockGuidanceService extends MockFactory {

  val mockGuidanceService: GuidanceService = mock[GuidanceService]

  object MockGuidanceService {

    def getStartPageUrl(processId: String): CallHandler[Future[String]] = {
      (mockGuidanceService
        .getStartPageUrl(_: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(processId, *, *)
    }

    def getPage(url: String, processId: String): CallHandler[Future[Option[Page]]] = {
      (mockGuidanceService
        .getPage(_: String, _: String)(_: HeaderCarrier, _: ExecutionContext))
        .expects(url, processId, *, *)
    }

  }

}
