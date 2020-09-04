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

import config.{AppConfig, ErrorHandler}
import javax.inject.{Inject, Singleton}
import play.api.i18n.Messages
import play.api.mvc._
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.errors._
import models.ui.{PageContext, StandardPage, QuestionPage, FormData}
import forms.NextPageFormProvider
import views.html.{standard_page, question_page}
import play.api.Logger
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    questionView: question_page,
    formProvider: NextPageFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger = Logger(getClass)

  def getPage(processCode: String, path: String): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)

    withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", _)).map {
      case Right(pageContext) =>
        logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}, answer = ${pageContext.answer}")
        pageContext.page match {
          case page: StandardPage => Ok(standardView(page, pageContext))
          case page: QuestionPage =>
            val form = pageContext.answer.fold(formProvider(questionName(path))) { answer =>
              formProvider(questionName(path)).bind(Map(questionName(path) -> answer))
            }
            Ok(questionView(page, pageContext, questionName(path), form))
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

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    formProvider(questionName(path)).bindFromRequest.fold(
      formWithErrors => {
        val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
        withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", _, Some(formData))).map {
          case Right(pageContext) =>
            pageContext.page match {
              case page: QuestionPage => BadRequest(questionView(page, pageContext, questionName(path), formWithErrors))
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
      nextPageUrl => {
        val redirectLocation  = routes.GuidanceController.getPage(processCode, nextPageUrl.url.drop(appConfig.baseUrl.length + processCode.length + 2))
        withExistingSession[Unit](service.saveAnswerToQuestion(_, s"/$path", nextPageUrl.url)).map {
          case Left(err) =>
            logger.error(s"Save Answer on page: $path failed, answser: /${redirectLocation.url}, error: $err")
            Redirect(redirectLocation)
          case Right(_) => Redirect(redirectLocation)
        }
      }
    )
  }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
