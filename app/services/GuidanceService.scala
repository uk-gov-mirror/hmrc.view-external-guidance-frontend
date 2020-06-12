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
import models.ui.{FormData, PageContext}
import play.api.Logger
import models.errors._
import models.RequestOutcome
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository
import models.ocelot.Process

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector, sessionRepository: SessionRepository, pageBuilder: PageBuilder, uiBuilder: UIBuilder) {
  val logger = Logger(getClass)

  def getPageContext(url: String, sessionId: String, formData: Option[FormData] = None)(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[PageContext]] =
    sessionRepository.get(sessionId).map {
      case Right(process) =>
        pageBuilder
          .pages(process)
          .fold(
            err => {
              logger.error(s"PageBuilder error $err for url $url on process ${process.meta.id} with sessionId $sessionId")
              Left(InvalidProcessError)
            },
            pages => {
              val stanzaIdToUrlMap: Map[String, String] = pages.map(page => (page.id, s"/guidance${page.url}")).toMap
              pages
                .find(page => page.url == url)
                .fold{
                  logger.error(s"Unable to find url $url within cached process ${process.meta.id} using sessionId $sessionId")
                  Left(BadRequestError): RequestOutcome[PageContext]
                } { pge =>
                  Right(PageContext(uiBuilder.fromStanzaPage(pge, formData)(stanzaIdToUrlMap), s"/guidance${pages.head.url}"))
                }
            }
          )
      case Left(err) =>
        logger.error(s"Repository returned $err, when attempting retrieve process using id $sessionId")
        Left(err)
    }

  def retrieveAndCacheScratch(uuid: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[String]] =
    retrieveAndCache(uuid, repositoryId, connector.scratchProcess)

  def retrieveAndCachePublished(processId: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[String]] =
    retrieveAndCache(processId, repositoryId, connector.publishedProcess)

  def retrieveAndCacheApproval(processId: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[String]] =
    retrieveAndCache(processId, repositoryId, connector.approvalProcess)

  def getStartPageUrl(processId: String, repositoryId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[String]] =
    retrieveAndCache(processId, repositoryId, connector.getProcess)

  private def retrieveAndCache(id: String, repositoryId: String, retrieveProcessById: String => Future[RequestOutcome[Process]])(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[String]] =
    retrieveProcessById(id).flatMap {
      case Right(process) =>
        sessionRepository.set(repositoryId, process).map {
          case Right(_) =>
            pageBuilder
              .pages(process)
              .fold(err => {
                logger.warn(s"Failed to parse process with error $err")
                Left(InvalidProcessError)
              }, pages => Right(pages.head.url))

          case Left(err) =>
            logger.warn(s"Failed to store new parsed process in session respository, $err")
            Left(err)
        }

      case Left(err) =>
        logger.warn(s"Unable to find process using id $id, error")
        Future.successful(Left(err))
    }
}
