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
import models.errors._
import models.RequestOutcome
import models.ui.{PageContext, StandardPage, QuestionPage, FormData}
import forms.NextPageFormProvider
import views.html.{standard_page, question_page}
import uk.gov.hmrc.http.SessionKeys
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class GuidanceController @Inject() (
    errorHandler: ErrorHandler,
    standardView: standard_page,
    questionView: question_page,
    formProvider: NextPageFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport {

  val logger = Logger(getClass)

  def getPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    withSession[PageContext](service.getPageContext(s"/$path", _)).map {
      case Right(pageContext) =>
        logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}")
        pageContext.page match {
          case page: StandardPage => Ok(standardView(page, pageContext.processStartUrl))
          case page: QuestionPage => Ok(questionView(page, pageContext.processStartUrl, questionName(path), formProvider(questionName(path))))
        }
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        NotFound(errorHandler.notFoundTemplate)
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest")
        BadRequest(errorHandler.badRequestTemplate)
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }

  def submitPage(path: String): Action[AnyContent] = Action.async { implicit request =>
    formProvider(questionName(path)).bindFromRequest.fold(
      formWithErrors => {
        val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
        withSession[PageContext](service.getPageContext(s"/$path", _, Some(formData))).map {
          case Right(pageContext) =>
            pageContext.page match {
              case page: QuestionPage => BadRequest(questionView(page, pageContext.processStartUrl, questionName(path), formWithErrors))
              case _ => BadRequest(errorHandler.badRequestTemplate)
            }
          case Left(NotFoundError) =>
            logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
            NotFound(errorHandler.notFoundTemplate)
          case Left(BadRequestError) =>
            logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
            BadRequest(errorHandler.badRequestTemplate)
          case Left(err) =>
            logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
            InternalServerError(errorHandler.internalServerErrorTemplate)
        }
      },
      nextPageUrl => Future.successful(Redirect(nextPageUrl.url))
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

  def published(processId: String): Action[AnyContent] = Action.async { implicit request =>
    val sessionId: String = hc.sessionId.fold(java.util.UUID.randomUUID.toString)(_.value)
    logger.info(s"Starting publish with sessionId = $sessionId")
    startUrlById(processId, sessionId, service.publishedProcess)
  }

  private def withSession[T](block: String => Future[RequestOutcome[T]])(implicit request: Request[_]): Future[RequestOutcome[T]] =
    hc.sessionId.fold {
      logger.error(s"Session Id missing from request when required")
      Future.successful(Left(BadRequestError): RequestOutcome[T])
    }(sessionId => block(sessionId.value))

  private def startUrlById(id: String, sessionId: String, processStartUrl: (String, String) => Future[RequestOutcome[String]])(
      implicit request: Request[_]
  ): Future[Result] =
    processStartUrl(id, sessionId).map {
      case Right(url) =>
        Redirect(s"/guidance$url").addingToSession(SessionKeys.sessionId -> sessionId)
      case Left(NotFoundError) =>
        logger.warn(s"Unable to find start page with id $id")
        NotFound(errorHandler.notFoundTemplate)
      case Left(err) =>
        logger.error(s"Error $err when trying to find start page with id $id")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
