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
import models.ui.{FormData, Page, PageContext}
import play.api.Logger
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository
import models.ocelot.Process

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector, sessionRepository: SessionRepository, pageBuilder: PageBuilder, uiBuilder: UIBuilder) {
  val logger: Logger = Logger(getClass)

  def getPage(url: String, sessionId: String, formData: Option[FormData] = None)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[PageContext]] =
    sessionRepository.get(sessionId).map { processOption =>
      processOption.flatMap { process =>
        pageBuilder
          .pages(process)
          .fold(
            err => {
              logger.info(s"PageBuilder error $err for url $url on process ${process.meta.id}")
              None
            },
            pages => {
              val stanzaIdToUrlMap: Map[String, String] = pages.map(page => (page.id, s"/guidance${page.url}")).toMap
              pages.find(page => page.url == url)
                   .map(pge => PageContext(uiBuilder.fromStanzaPage(pge, formData)(stanzaIdToUrlMap), pages.head.url))
            }
          )
      }
    }

  def scratchProcess(uuid: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[String]] =
    startProcessView(uuid, repositoryId, connector.scratchProcess)

  def getStartPageUrl(processId: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[String]] =
    startProcessView(processId, repositoryId, connector.getProcess)

  private def startProcessView(id: String, repositoryId: String, processById: String => Future[Option[Process]])(
      implicit hc: HeaderCarrier,
      context: ExecutionContext
  ): Future[Option[String]] =
    processById(id).flatMap {
      case Some(process) =>
        sessionRepository.set(repositoryId, process).map { _ =>
          pageBuilder.pages(process).fold(_ => None, pages => Some(pages.head.url))
        }
      case None => Future.successful(None)
    }
}
