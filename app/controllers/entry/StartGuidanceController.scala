/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import core.models.errors._
import core.models.RequestOutcome
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction

@Singleton
class StartGuidanceController @Inject() (
    errorHandler: ErrorHandler,
    service: GuidanceService,
    sessionIdAction: SessionIdAction,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport {

  val logger = Logger(getClass)

  def scratch(uuid: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting scratch journey")
    retrieveCacheAndRedirectToView(uuid, service.retrieveAndCacheScratch)
  }

  def approval(processId: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting approval direct view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApproval)
  }

  def approvalPage(processId: String, url: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting approval direct page view journey")
    retrieveCacheAndRedirectToView(processId, service.retrieveAndCacheApprovalByPageUrl(s"/$url"))
  }

  def published(processCode: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"Starting publish journey for $processCode")
    retrieveCacheAndRedirectToView(processCode, service.retrieveAndCachePublished)
  }

  private def retrieveCacheAndRedirectToView(id: String, retrieveAndCache: (String, String) => Future[RequestOutcome[(String,String)]])(
      implicit request: Request[_]
  ): Future[Result] = {
    val (sessionId, egNewSessionId) = existingOrNewSessionId()
    logger.info(s"Calling Retrieve and cache service for process $id using sessionId = $sessionId, EG = ${egNewSessionId}")
    retrieveAndCache(id, sessionId).map {
      case Right((url, processCode)) =>
        val target = controllers.routes.GuidanceController.getPage(processCode, url.drop(1), None).url
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
