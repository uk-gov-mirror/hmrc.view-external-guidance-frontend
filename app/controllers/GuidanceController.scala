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
import play.api.data.{Form}
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.errors._
import models.{PageContext, FormEvaluationContext}
import models.ui.{FormPage, StandardPage}
import forms.SubmittedAnswerFormProvider
import views.html.{form_page, standard_page}
import play.api.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction
import play.twirl.api.Html

import scala.concurrent.Future

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    formView: form_page,
    formProvider: SubmittedAnswerFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger: Logger = Logger(getClass)

  def getPage(processCode: String, path: String, p: Option[String]): Action[AnyContent] = sessionIdAction.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    println(s"GETPAGE: $path")
    withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", p.isDefined, _)).flatMap {
      case Right(pageContext) =>
        logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}," +
                    s" answer = ${pageContext.answer}, backLink = ${pageContext.backLink}")
        pageContext.page match {
          case page: StandardPage => service.saveLabels(pageContext.sessionId, pageContext.labels).map {
              case Right(_) => Ok(standardView(page, pageContext))
              case Left(err) => InternalServerError(errorHandler.internalServerErrorTemplate)
            }
          case page: FormPage =>
            println(s"FormPage $page")
            Future.successful(Ok(formView(page, pageContext, formInputName(path), populatedForm(pageContext, path))))
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
    println(s"SUBMITPAGE: $path")
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[FormEvaluationContext](service.getFormEvaluationContext(processCode, s"/$path", previousPageByLink = false, _)).flatMap {
      case Right(evalContext) =>
        val form = formProvider(formInputName(path))
        form.bindFromRequest.fold(
          formWithErrors => {
            //val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
            Future.successful(BadRequest(createInputView(evalContext, formInputName(path), formWithErrors)))
          },
          submittedAnswer => {
            service.validateUserResponse(evalContext, submittedAnswer.text).fold {
              // Answer didn't pass page DataInput stanza validation
              //val formData = FormData(path, Map(), Seq(FormError("", "error.required")))
              Future.successful(BadRequest(createInputView(evalContext,
                formInputName(path),
                form.bind(Map(formInputName(path) -> submittedAnswer.text)))))
            } { answer =>
              service.submitPage(evalContext, s"/$path", answer, submittedAnswer.text).map {
                case Right((None, labels)) =>
                  // No valid next page id indicates the guidance has determined the page should be re-displayed (probably due to an error)
                  logger.info(s"Post submit page evaluation indicates guidance detected input error")
                  BadRequest(createInputView(evalContext.copy(labels = labels), formInputName(path), form))
                case Right((Some(stanzaId), _)) =>
                  // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                  val url = evalContext.stanzaIdToUrlMap(stanzaId)
                  logger.info(s"Post submit page evaluation indicates next page at stanzaId: $stanzaId => $url")
                  Redirect(routes.GuidanceController.getPage(
                    processCode,
                    url.drop(appConfig.baseUrl.length + processCode.length + 2),
                    previousPageQueryString(url, evalContext.backLink)))
                case Left(err) =>
                  logger.error(s"Page submission failed: $err")
                  InternalServerError(errorHandler.internalServerErrorTemplate)
              }
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

  private def createInputView(fec: FormEvaluationContext, inputName: String, form: Form[_])(implicit request: Request[_], messages: Messages): Html = {
    val ctx = service.getPageContext(fec)
    ctx.page match {
      case page: FormPage => formView(page, ctx, inputName, form)
      case _ => errorHandler.badRequestTemplateWithProcessCode(Some(ctx.processCode))
    }
  }

  private def populatedForm(ctx: PageContext, path: String): Form[_] = ctx.answer.fold(formProvider(formInputName(path))) { answer =>
    formProvider(formInputName(path)).bind(Map(formInputName(path) -> answer))
  }

  private def formInputName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
  private def previousPageQueryString(url: String, backLink: Option[String]): Option[String] = backLink.collect{case bl if bl == url => "1"}
}
