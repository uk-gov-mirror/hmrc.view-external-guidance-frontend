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
import models.ui.{PageEvaluationContext, StandardPage, QuestionPage, FormData}
import forms.NextPageFormProvider
import views.html.{standard_page, question_page}
import play.api.Logger
import play.twirl.api.Html
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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

    withExistingSession[Html]{sessionId =>
      service.getPageContext(processCode, s"/$path", sessionId).map {
        case Right(pageContext) =>
          logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}," +
                      s" answer = ${pageContext.answer}, backLink = ${pageContext.backLink}")
          pageContext.page match {
            case page: StandardPage =>
              service.saveLabels(sessionId, pageContext.labels)
              Right(standardView(page, pageContext))
            case page: QuestionPage =>
              val form = pageContext.answer.fold(formProvider(questionName(path))) { answer =>
                formProvider(questionName(path)).bind(Map(questionName(path) -> answer))
              }
              Right(questionView(page, pageContext, questionName(path), form))
          }
        case Left(err) => Left(err)
      }
    }.map{
      case Right(view) => Ok(view)
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode)))
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest")
        BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }

  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[Result]{sessionId =>
      service.getPageEvaluationContext(processCode, s"/$path", sessionId).flatMap {
        case Right(evalContext) =>
          formProvider(questionName(path)).bindFromRequest.fold(
            formWithErrors => {
              val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
              val pageContext = service.getPageContext(evalContext, Some(formData))
              pageContext.page match {
                case page: QuestionPage => Future.successful(Right(BadRequest(questionView(page, pageContext, questionName(path), formWithErrors))))
                case _ => Future.successful(Right(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))))
              }
            },
            nextPageUrl => {
              // nextPageUrl will become the value of the question or the input field data
              // Get the page context and process the Question or Input stanzas amd any following stanzas with the nextPageUrl value
              // If this processing indicates a return to a stanza seen before the question or input, it implies a reshowing of the
              // current page
              val redirectLocation  = routes.GuidanceController.getPage(processCode, nextPageUrl.url.drop(appConfig.baseUrl.length + processCode.length + 2))
              service.saveAnswerToQuestion(sessionId, s"/$path", nextPageUrl.url).map{
                case Left(err) =>
                  logger.error(s"Save Answer on page: $path failed, answser: /${redirectLocation.url}, error: $err")
                  Right(Redirect(redirectLocation))
                case Right(_) => Right(Redirect(redirectLocation))
              }
            }
          )

        case Left(NotFoundError) =>
          logger.warn(s"Request for PageEvaluationContext at /$path returned NotFound during form submission, returning NotFound")
          Future.successful(Right(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode)))))
        case Left(BadRequestError) =>
          logger.warn(s"Request for PageEvaluationContext at /$path returned BadRequest during form submission, returning BadRequest")
          Future.successful(Right(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))))
        case Left(err) =>
          logger.error(s"Request for PageEvaluationContext at /$path returned $err during form submission, returning InternalServerError")
          Future.successful(Right(InternalServerError(errorHandler.internalServerErrorTemplate)))
      }
    }.map{
      case Right(result) => result
      case Left(err) =>
        logger.warn(s"Request for PageEvaluationContext at /$path returned BadRequest during form submission, returning BadRequest")
        BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))
    }

  }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
