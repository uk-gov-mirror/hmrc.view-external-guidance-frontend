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
import play.api.Logger
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector, sessionRepository: SessionRepository) {

  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
    connector.scratchProcess(uuid) map { process =>
      Logger.info(s"scratch: process returned, meta: ${process.meta}")
      PageBuilder.pages(process) match {
        case Right(Nil) => ""
        case Right(pages) => 
          Logger.info(s"scratch: return Url of start page: ${pages.head.url}")
          pages.head.url
        case Left(_) => ""
      }
    }

  def getStartPageUrl(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] = {
    connector.getProcess(processId) map { process =>
      PageBuilder.pages(process) match {
        case Right(pages) =>
          val startPage = pages.find(page => page.id == "start")
          startPage.fold("")(page => page.url)

        // TODO
        case Left(_) => ""
      }
    }
  }

  def getPage(url: String, processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Page]] = {
    connector.getProcess(processId) map { process =>
      PageBuilder.pages(process) match {
        case Right(pages) =>
          val stanzaIdToUrlMap = pages
            .map(page => (page.id, s"/guidance${page.url}"))
            .toMap

          pages
            .find(page => page.url == url)
            .map(page => UIBuilder.fromStanzaPage(page)(stanzaIdToUrlMap))

        // TODO
        case Left(_) =>
          None
      }
    }
  }
}
