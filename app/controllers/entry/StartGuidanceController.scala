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

package controllers.entry

import config.ErrorHandler
import javax.inject.{Inject, Singleton}
import play.api.i18n.Messages
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.errors._
import models.RequestOutcome
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.SessionFrontendController
import controllers.actions.SessionIdAction
import views.html.labels_page
import repositories.ProcessContext

@Singleton
class StartGuidanceController @Inject() (
    errorHandler: ErrorHandler,
    service: GuidanceService,
    sessionIdAction: SessionIdAction,
    labelsView: labels_page,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with SessionFrontendController {

  val logger = Logger(getClass)

  def scratch(uuid: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting scratch journey")
    retrieveCacheAndRedirectToView(uuid, service.retrieveAndCacheScratch)
  }

  def published(processCode: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting publish journey for $processCode")
    retrieveCacheAndRedirectToView(processCode, service.retrieveAndCachePublished)
  }

  def approval(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting approval direct view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApproval)
  }

  def approvalPage(processId: String, url: String): Action[AnyContent] = Action.async { implicit request =>
    def retrieveCacheAndRedirect(startUrl: String)(processId: String, repositoryId: String): Future[RequestOutcome[(String,String)]] =
      service.retrieveAndCacheApproval(processId, repositoryId).map {
        case Right((_,processCode)) => Right((startUrl,processCode))
        case err @ Left(_) => err
      }

    logger.info(s"Starting approval direct page view journey")
    retrieveCacheAndRedirectToView(processId, retrieveCacheAndRedirect(s"/$url"))
  }

  def labels: Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[ProcessContext](service.getProcessContext(_)).flatMap {
      case Right(ctx) =>
        for(k <- ctx.labels.keys) {
          println(s"$k -> ${ctx.labels(k)}")
        }
        Future.successful(Ok(labelsView(ctx.labels)))
      case Left(NotFoundError) =>
        logger.warn(s"Request for ProcessContext returned NotFound, returning NotFound")
        Future.successful(NotFound(errorHandler.internalServerErrorTemplate))
      case Left(BadRequestError) =>
        logger.warn(s"Request for ProcessContext returned BadRequest")
        Future.successful(BadRequest(errorHandler.internalServerErrorTemplate))
      case Left(err) =>
        logger.error(s"Request for ProcessContext returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }


  private def retrieveCacheAndRedirectToView(id: String, retrieveAndCache: (String, String) => Future[RequestOutcome[(String,String)]])(
      implicit request: Request[_]
  ): Future[Result] = {
    val (sessionId, egNewSessionId) = existingOrNewSessionId()
    logger.info(s"Calling Retrieve and cache service for process $id using sessionId = $sessionId, EG = ${egNewSessionId}")
    retrieveAndCache(id, sessionId).map {
      case Right((url, processCode)) =>
        val target = controllers.routes.GuidanceController.getPage(processCode, url.drop(1)).url
        logger.warn(s"Redirecting to begin viewing process $id/$processCode at ${target} using sessionId $sessionId, EG_NEW_SESSIONID = $egNewSessionId")
        egNewSessionId.fold(Redirect(target))(newId => Redirect(target).addingToSession((sessionIdAction.EgNewSessionIdName -> newId)))
      case Left(NotFoundError) =>
        logger.warn(s"Unable to find process $id and render using sessionId $sessionId")
        NotFound(errorHandler.notFoundTemplate)
      case Left(err) =>
        logger.error(s"Error $err returned from Guidance service when trying to access process $id using sessionId $sessionId")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }

  private def existingOrNewSessionId()(implicit request: Request[_]): (String, Option[String]) =
    hc.sessionId.fold[(String, Option[String])]{
      val id = s"session-${java.util.UUID.randomUUID.toString}"
      (id, Some(id))
    }(sessionId => (sessionId.value, None))
}
