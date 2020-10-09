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
import models.{PageContext, PageEvaluationContext}
import models.ui.{StandardPage, QuestionPage, FormData}
import forms.SubmittedAnswerFormProvider
import views.html.{standard_page, question_page}
import play.api.Logger
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    questionView: question_page,
    formProvider: SubmittedAnswerFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger = Logger(getClass)

  def getPage(processCode: String, path: String): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)

    withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", _)).flatMap {
      case Right(pageContext) =>
        logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}," +
                    s" answer = ${pageContext.answer}, backLink = ${pageContext.backLink}")

        pageContext.page match {
          case page: StandardPage =>
            service.saveLabels(pageContext.sessionId, pageContext.labels).map{
              case Right(_) => Ok(standardView(page, pageContext))
              case Left(err) =>
                logger.error(s"Failed to save labels after evaluation of a standard page")
                InternalServerError(errorHandler.internalServerErrorTemplate)
            }
          case page: QuestionPage =>
            // Original
            val form = pageContext.answer.fold(formProvider(questionName(path))) { answer =>
              formProvider(questionName(path)).bind(Map(questionName(path) -> answer))
            }

            Future.successful(Ok(questionView(page, pageContext, questionName(path), form)))
        }
      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound, returning NotFound")
        Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest")
        Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[PageEvaluationContext](service.getPageEvaluationContext(processCode, s"/$path", _)).flatMap {
      case Right(evalContext) =>
        val form = formProvider(questionName(path))
        form.bindFromRequest.fold(
          formWithErrors => {
            val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
            val pageContext = service.getPageContext(evalContext, Some(formData))
            pageContext.page match {
              case page: QuestionPage => Future.successful(BadRequest(questionView(page, pageContext, questionName(path), formWithErrors)))
              case _ => Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
            }
          },
          submittedAnswer => {
            service.submitPage(evalContext, s"/$path", submittedAnswer.text).map{
              case Left(err) =>
                logger.error(s"Page submission failed: $err")
                InternalServerError(errorHandler.internalServerErrorTemplate)
              case Right((None, labels)) =>
                // None here indeicates there is no valid next page id because the guidance redirect back to a redisplay of page
                logger.info(s"Post submit page evaluation indicates guidance detected input error")
                val formData = FormData(path, form.data, form.errors)
                val pageContext = service.getPageContext(evalContext.copy(labels = labels), Some(formData))
                pageContext.page match {
                  case page: QuestionPage => BadRequest(questionView(page, pageContext, questionName(path), form))
                  case _ => BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode)))
                }

              case Right((Some(stanzaId), _)) =>
                // Some(stanzaId) here idicates a redirect to the page with id "stanzaId", url = evalContext.stanzaIdMap(stanzaId).url
                val url = evalContext.stanzaIdToUrlMap(stanzaId)
                logger.info(s"Post submit page evaluation indicates next page at stanzaId: $stanzaId => $url")
                val redirect  = routes.GuidanceController.getPage(processCode, url.drop(appConfig.baseUrl.length + processCode.length + 2))
                logger.info(s"Redirecting => $redirect")
                Redirect(redirect)
            }
          }
        )

      case Left(NotFoundError) =>
        logger.warn(s"Request for PageContext at /$path returned NotFound during form submission, returning NotFound")
        Future.successful(NotFound(errorHandler.notFoundTemplateWithProcessCode(Some(processCode))))
      case Left(BadRequestError) =>
        logger.warn(s"Request for PageContext at /$path returned BadRequest during form submission, returning BadRequest")
        Future.successful(BadRequest(errorHandler.badRequestTemplateWithProcessCode(Some(processCode))))
      case Left(err) =>
        logger.error(s"Request for PageContext at /$path returned $err during form submission, returning InternalServerError")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
    }
  }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
