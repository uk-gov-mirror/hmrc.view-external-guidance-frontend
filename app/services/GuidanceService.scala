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

import config.AppConfig
import connectors.GuidanceConnector
import javax.inject.{Inject, Singleton}
import models.{PageContext, PageEvaluationContext}
import play.api.Logger
import models.errors.{BadRequestError, InvalidProcessError, InternalServerError}
import models.RequestOutcome
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import repositories.{ProcessContext, SessionRepository}
import models.ocelot.{LabelCache, Labels, Process}

@Singleton
class GuidanceService @Inject() (
    appConfig: AppConfig,
    connector: GuidanceConnector,
    sessionRepository: SessionRepository,
    pageBuilder: PageBuilder,
    pageRenderer: PageRenderer,
    uiBuilder: UIBuilder
) {
  val logger = Logger(getClass)

  def getProcessContext(sessionId: String): Future[RequestOutcome[ProcessContext]] = sessionRepository.get(sessionId)

  def getProcessContext(sessionId: String, pageUrl: String, previousPageByLink: Boolean): Future[RequestOutcome[ProcessContext]] =
    sessionRepository.get(sessionId, pageUrl, previousPageByLink)

  def getPageEvaluationContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[PageEvaluationContext]] =
    getProcessContext(sessionId, s"${processCode}$url", previousPageByLink).map {
      case Right(ProcessContext(process, answers, labelsMap, urlToPageId, backLink)) if process.meta.processCode == processCode =>
        urlToPageId.get(url).fold[RequestOutcome[PageEvaluationContext]]{
          logger.error(s"Unable to find url $url within cached process ${process.meta.id} using sessionId $sessionId")
          Left(BadRequestError)
        }{ pageId =>
          pageBuilder.buildPage(pageId, process).fold(
            err => {
              logger.error(s"PageBuilder error $err on process ${process.meta.id} with sessionId $sessionId")
              Left(InvalidProcessError)
            },
            page => {
              val stanzaIdToUrlMap = urlToPageId.map{case (k, v) => (v, s"${appConfig.baseUrl}/${processCode}${k}")}.toMap
              val (visualStanzas, labels, dataInput) = pageRenderer.renderPage(page, LabelCache(labelsMap))
              val uiPage = uiBuilder.buildPage(page.url, visualStanzas)(stanzaIdToUrlMap)

              Right(
                PageEvaluationContext(
                  uiPage,
                  page,
                  dataInput,
                  sessionId,
                  stanzaIdToUrlMap,
                  process.startUrl.map( startUrl => s"${appConfig.baseUrl}/${processCode}${startUrl}"),
                  process.title,
                  process.meta.id,
                  processCode,
                  labels,
                  backLink.map(bl => s"${appConfig.baseUrl}/$bl"),
                  answers.get(url)
                )
              )
            }
          )
        }
      case Right(ProcessContext(_,_,_,_,_)) =>
        logger.error(s"Referenced session ( $sessionId ) does not contain a process with processCode $processCode")
        Left(InternalServerError)
      case Left(err) =>
        logger.error(s"Repository returned $err, when attempting retrieve process using id (sessionId) $sessionId")
        Left(err)
    }

  def getPageContext(pec: PageEvaluationContext): PageContext = {
    val (visualStanzas, labels, dataInput) = pageRenderer.renderPage(pec.page, pec.labels)
    val uiPage = uiBuilder.buildPage(pec.page.url, visualStanzas)(pec.stanzaIdToUrlMap)
    PageContext(pec.copy(dataInput = dataInput), uiPage, labels)
  }

  def getPageContext(processCode: String, url: String, previousPageByLink: Boolean, sessionId: String)
                    (implicit context: ExecutionContext): Future[RequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, previousPageByLink, sessionId).map{
      case Right(evalContext) => Right(PageContext(evalContext))
      case Left(err) => Left(err)
    }

  def validateUserResponse(evalContext: PageEvaluationContext, response: String): Option[String] =
    evalContext.dataInput.fold[Option[String]](None)(_.validInput(response))

  def submitPage(evalContext: PageEvaluationContext, url: String, validatedAnswer: String, submittedAnswer: String)
                (implicit context: ExecutionContext): Future[RequestOutcome[(Option[String], Labels)]] = {
    val (optionalNext, labels) = pageRenderer.renderPagePostSubmit(evalContext.page, evalContext.labels, validatedAnswer)
    optionalNext.fold[Future[RequestOutcome[(Option[String], Labels)]]](Future.successful(Right((None, labels)))){next =>
      logger.info(s"Next page found at stanzaId: $next")
      sessionRepository.saveUserAnswerAndLabels(evalContext.sessionId, url, submittedAnswer, labels.updatedLabels.values.toSeq).map{
        case Left(err) =>
          logger.error(s"Failed to save updated labels, error = $err")
          Left(InternalServerError)
        case Right(_) => Right((Some(next), labels))
      }
    }
  }

  def saveLabels(docId: String, labels: Labels): Future[RequestOutcome[Unit]] =
    labels.updatedLabels.values.headOption.fold[Future[RequestOutcome[Unit]]](Future.successful(Right({})))(_ =>
      sessionRepository.saveLabels(docId, labels.updatedLabels.values.toSeq)
    )

  def retrieveAndCacheScratch(uuid: String, docId: String)
                             (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(
      uuid,
      docId,
      { uuidAsProcessId => connector.scratchProcess(uuidAsProcessId).map{
        case Right(process: Process) => Right(process.copy(meta = process.meta.copy(id = uuidAsProcessId)))
        case err @ Left(_) => err
      }}
    )

  def retrieveAndCachePublished(processCode: String, docId: String)
                               (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processCode, docId, connector.publishedProcess)

  def retrieveAndCacheApproval(processId: String, docId: String)
                              (implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, connector.approvalProcess)

  private def retrieveAndCache(processIdentifier: String, docId: String, retrieveProcessById: String => Future[RequestOutcome[Process]])(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[(String,String)]] =
    retrieveProcessById(processIdentifier).flatMap {
      case Left(err) =>
        logger.warn(s"Unable to find process using identifier $processIdentifier, received $err")
        Future.successful(Left(err))

      case Right(process) =>
        pageBuilder.pages(process).fold(err => {
            logger.warn(s"Failed to parse process with error $err")
            Future.successful(Left(InvalidProcessError))
        }, pages =>
          sessionRepository.set(docId, process, pages.map(p => (p.url -> p.id)).toMap).map {
            case Right(_) => Right((pages.head.url, process.meta.processCode))
            case Left(err) =>
              logger.error(s"Failed to store new parsed process in session respository, $err")
              Left(err)
          }
        )
    }

}
