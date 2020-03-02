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

import connectors.GuidanceConnector
import javax.inject.{Inject, Singleton}
import models.ui.Page
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector) {

  def getStartPageUrl(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] = {
    connector.getProcess(processId) map { process =>
      PageBuilder.pages(process) match {
        case Right(pages) =>
          val startPage = pages.find(p => p.id == "start")
          startPage.fold("")(p => p.url)
      }
    }
  }

  def getPage(url: String, processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Page]] = {
    connector.getProcess(processId) map { process =>
      PageBuilder.pages(process) match {
        case Right(pages) => UIBuilder.pages(pages).get(url)
      }
    }
  }
}
