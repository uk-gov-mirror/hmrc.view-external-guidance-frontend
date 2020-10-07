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
import models.ui.FormData
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

  def getProcessContext(sessionId: String, pageUrl: String): Future[RequestOutcome[ProcessContext]] = sessionRepository.get(sessionId, pageUrl)

  def getPageEvaluationContext(processCode: String, url: String, sessionId: String)(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[PageEvaluationContext]] =
    getProcessContext(sessionId, s"${processCode}$url").map {
      case Right(ProcessContext(process, answers, labelsMap, backLink)) if process.meta.processCode == processCode =>
        pageBuilder
          .pages(process)
          .fold(
            err => {
              logger.error(s"PageBuilder error $err on process ${process.meta.id} with sessionId $sessionId")
              Left(InvalidProcessError)
            },
            pages =>
              pages
                .find(_.url == url)
                .fold {
                  logger.error(s"Unable to find url $url within cached process ${process.meta.id} using sessionId $sessionId")
                  Left(BadRequestError): RequestOutcome[PageEvaluationContext]
                } { pge =>
                  Right(
                    PageEvaluationContext(
                      pge,
                      sessionId,
                      pages.map(p => (p.id, s"${appConfig.baseUrl}/${processCode}${p.url}")).toMap,
                      process.startUrl.map( startUrl => s"${appConfig.baseUrl}/${processCode}${startUrl}"),
                      process.title,
                      process.meta.id,
                      processCode,
                      LabelCache(labelsMap),
                      backLink.map(bl => s"${appConfig.baseUrl}/$bl"),
                      answers.get(url)
                    )
                  )
                }
          )
      case Right(ProcessContext(_,_,_,_)) =>
        logger.error(s"Referenced session ( $sessionId ) does not contain a process with processCode $processCode")
        Left(InternalServerError)
      case Left(err) =>
        logger.error(s"Repository returned $err, when attempting retrieve process using id (sessionId) $sessionId")
        Left(err)
    }

  def getPageContext(pec: PageEvaluationContext, formData: Option[FormData] = None): PageContext = {
    val (visualStanzas, labels, _) = pageRenderer.renderPage(pec.page, pec.labels)
    val uiPage = uiBuilder.fromStanzas(pec.page.url, visualStanzas, formData)(pec.stanzaIdToUrlMap)
    PageContext(pec, uiPage, labels)
  }

  def getPageContext(processCode: String, url: String, sessionId: String)(implicit context: ExecutionContext): Future[RequestOutcome[PageContext]] =
    getPageEvaluationContext(processCode, url, sessionId).map{
      case Right(evalContext) => Right(getPageContext(evalContext))
      case Left(err) => Left(err)
    }

  def submitPage(evalContext: PageEvaluationContext, url: String, answer: String)(
        implicit context: ExecutionContext
    ): Future[RequestOutcome[Option[String]]] =
    saveAnswerToQuestion(evalContext.sessionId, url, answer).flatMap{
      case Left(err) =>
        logger.error(s"Failed to saved answers, error = $err")
        Future.successful(Left(InternalServerError))
      case Right(_) =>
        pageRenderer
          .renderPagePostSubmit(evalContext.page, evalContext.labels, answer)
          .fold[Future[RequestOutcome[Option[String]]]](Future.successful(Right(None))){
            case (next, updateLabels) =>
              logger.info(s"Next page found at stanzaId: $next")
              saveLabels(evalContext.sessionId, updateLabels).map{
                case Left(err) =>
                  logger.error(s"Failed to save updated labels, error = $err")
                  Left(InternalServerError)
                case Right(_) => Right(Some(next))
              }
          }

    }

  def saveLabels(docId: String, labels: Labels): Future[RequestOutcome[Unit]] =
    sessionRepository.saveLabels(docId, labels)

  def saveAnswerToQuestion(docId: String, url: String, answer: String): Future[RequestOutcome[Unit]] =
    sessionRepository.saveAnswerToQuestion(docId, url, answer)

  def retrieveAndCacheScratch(uuid: String, docId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(
      uuid,
      docId,
      { uuidAsProcessId => connector.scratchProcess(uuidAsProcessId).map{
        case Right(process: Process) => Right(process.copy(meta = process.meta.copy(id = uuidAsProcessId)))
        case err @ Left(_) => err
      }}
    )

  def retrieveAndCachePublished(processCode: String, docId: String)(implicit hc: HeaderCarrier, context: ExecutionContext):
  Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processCode, docId, connector.publishedProcess)

  def retrieveAndCacheApproval(processId: String, docId: String)(implicit hc: HeaderCarrier, context: ExecutionContext):
  Future[RequestOutcome[(String,String)]] =
    retrieveAndCache(processId, docId, connector.approvalProcess)

  private def retrieveAndCache(processIdentifier: String, docId: String, retrieveProcessById: String => Future[RequestOutcome[Process]])(
      implicit context: ExecutionContext
  ): Future[RequestOutcome[(String,String)]] =
    retrieveProcessById(processIdentifier).flatMap {
      case Left(err) =>
        logger.warn(s"Unable to find process using identifier $processIdentifier, error")
        Future.successful(Left(err))

      case Right(process) =>
        pageBuilder.pages(process).fold(err => {
            logger.warn(s"Failed to parse process with error $err")
            Future.successful(Left(InvalidProcessError))
        }, pages =>
          sessionRepository.set(docId, process, uniqueLabels(pages).map(l => (l.name, l)).toMap).map {
            case Right(_) => Right((pages.head.url, process.meta.processCode))
            case Left(err) =>
              logger.error(s"Failed to store new parsed process in session respository, $err")
              Left(err)
          }
        )
    }
}
