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

package controllers

import config.ErrorHandler
import javax.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.ui.{Page, StandardPage, QuestionPage}
import forms.QuestionFormProvider
import views.html.{render_standard_page, render_question_page}
import uk.gov.hmrc.http.SessionKeys
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class GuidanceController @Inject() (
    errorHandler: ErrorHandler,
    standardView: render_standard_page,
    questionView: render_question_page,
    formProvider: QuestionFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport {
  val logger: Logger = Logger( getClass )

  def getPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"getPage: $path")
    sessionPage(path).map {
        case Some(page: StandardPage) => Ok(standardView(page))
        case Some(page: QuestionPage) =>
          formProvider().bindFromRequest.fold( 
            formWithErrors => Ok(questionView(page, formWithErrors)),
            value => Ok(questionView(page, formProvider()))
          )
        case None => NotFound(errorHandler.notFoundTemplate)
    }
  }

  def submitPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    logger.info(s"submitPage: $path")
    formProvider().bindFromRequest.fold( 
      formWithErrors => {
        sessionPage(path.dropWhile(_ == '/')).map {
            case Some(page: QuestionPage) => BadRequest(questionView(page, formWithErrors))
            case _ => NotFound(errorHandler.notFoundTemplate)
        }
      },
      value => Future.successful(Redirect(value))
    )
  }

  def startJourney(processId: String): Action[AnyContent] = Action.async { implicit request =>
    val sessionId: String = hc.sessionId.fold(java.util.UUID.randomUUID.toString)(_.value)
    logger.info(s"Starting journey with sessionId = $sessionId")
    startUrlById(processId, sessionId, service.getStartPageUrl)
  }

  def scratch(uuid: String): Action[AnyContent] = Action.async { implicit request =>
    val sessionId: String = hc.sessionId.fold(java.util.UUID.randomUUID.toString)(_.value)
    logger.info(s"Starting scratch with sessionId = $sessionId")
    startUrlById(uuid, sessionId, service.scratchProcess)
  }

  private def sessionPage(path: String)(implicit request: Request[_]): Future[Option[Page]] =
    request.session.get(SessionKeys.sessionId) match {
      case Some(sessionId) => service.getPage(s"/$path", sessionId)
      case None => Future.successful(None)
    }

  private def startUrlById( id: String, sessionId: String, processStartUrl: (String, String) => Future[Option[String]])
                          (implicit request: Request[_]): Future[Result] =
    processStartUrl(id, sessionId).map { urlOption =>
      urlOption.fold(NotFound(errorHandler.notFoundTemplate))(url => {
          val relativeUrl = s"/guidance$url"
          Redirect(relativeUrl).withSession(SessionKeys.sessionId -> sessionId)
        }
      )
    }
}
