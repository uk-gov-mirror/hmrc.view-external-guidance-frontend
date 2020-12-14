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
import play.api.data.{Form, FormError}
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import models.errors._
import models.{PageContext, PageEvaluationContext}
import models.ui.{DateInputPage, FormData, InputPage, QuestionPage, StandardPage}
import models.ocelot.stanzas.DataInput
import forms.SubmittedAnswerFormProvider
import views.html.{input_date_page, input_page, question_page, standard_page}
import play.api.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import controllers.actions.SessionIdAction
import play.twirl.api.Html

import scala.concurrent.Future
import models.ocelot.KeyedStanza

@Singleton
class GuidanceController @Inject() (
    appConfig: AppConfig,
    sessionIdAction: SessionIdAction,
    errorHandler: ErrorHandler,
    standardView: standard_page,
    questionView: question_page,
    inputView: input_page,
    dateInputView: input_date_page,
    formProvider: SubmittedAnswerFormProvider,
    service: GuidanceService,
    mcc: MessagesControllerComponents
) extends FrontendController(mcc)
  with SessionFrontendController {

  val logger: Logger = Logger(getClass)

  def getPage(processCode: String, path: String, p: Option[String]): Action[AnyContent] = sessionIdAction.async { implicit request =>
    withExistingSession[PageContext](service.getPageContext(processCode, s"/$path", p.isDefined, _)).flatMap {
      case Right(pageContext) =>
        showPage(path, pageContext)
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

  private def showPage(path: String, pageContext: PageContext)(implicit request:Request[AnyContent]): Future[Result] = {
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    logger.info(s"Retrieved page at ${pageContext.page.urlPath}, start at ${pageContext.processStartUrl}," +
      s" answer = ${pageContext.answer}, backLink = ${pageContext.backLink}")

    pageContext.page match {
      case page: StandardPage =>
        service.saveLabels(pageContext.sessionId, pageContext.labels).map {
          case Right(_) => Ok(standardView(page, pageContext))
          case Left(err) => InternalServerError(errorHandler.internalServerErrorTemplate)
        }
      case page: QuestionPage =>
        Future.successful(Ok(questionView(page, pageContext, questionName(path), populatedForm(pageContext, path))))
      case page: InputPage =>
        Future.successful(Ok(inputView(page, pageContext, questionName(path), populatedForm(pageContext, path))))
      case page: DateInputPage =>
        Future.successful(Ok(dateInputView(page, pageContext, questionName(path), populatedForm(pageContext, path))))
    }
  }


  def submitPage(processCode: String, path: String): Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[PageEvaluationContext](service.getPageEvaluationContext(processCode, s"/$path", previousPageByLink = false, _)).flatMap {
      case Right(evalContext) =>
        val form = formProvider(questionName(path))
        form.bindFromRequest.fold(
          formWithErrors => {
            val formData = FormData(path, formWithErrors.data, formWithErrors.errors)
            Future.successful(BadRequest(createInputView(evalContext, questionName(path), Some(formData), formWithErrors)))
          },
          submittedAnswer => {
            validateAnswer(evalContext, submittedAnswer.text).fold{
              // Answer didnt pass page DataInput stanza validation
              val formData = FormData(path, Map(), Seq(FormError("","error.required")))
              Future.successful(BadRequest(createInputView(evalContext,
                                                           questionName(path),
                                                           Some(formData),
                                                           form.bind(Map(questionName(path) -> submittedAnswer.text)))))
            }{ answer =>
              service.submitPage(evalContext, s"/$path", answer, submittedAnswer.text).map{
                case Right((None, labels)) =>
                  // No valid next page id indicates the guidance has determined there is an error and page should be re-displayed
                  logger.info(s"Post submit page evaluation indicates guidance detected input error")
                  BadRequest(createInputView(evalContext.copy(labels = labels), questionName(path), None, form))
                case Right((Some(stanzaId), _)) =>
                  // Some(stanzaId) here indicates a redirect to the page with id "stanzaId"
                  val url = evalContext.stanzaIdToUrlMap(stanzaId)
                  logger.info(s"Post submit page evaluation indicates next page at stanzaId: $stanzaId => $url")
                  Redirect(routes.GuidanceController.getPage(processCode, url.drop(appConfig.baseUrl.length + processCode.length + 2), None))
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

  private def validateAnswer(ctx: PageEvaluationContext, answer: String): Option[String] =
    ctx.page.keyedStanzas.collect{case KeyedStanza(_, i: DataInput) => i.validInput(answer)}.flatten.headOption

  private def createInputView(pec: PageEvaluationContext, inputName: String, formData: Option[FormData], form: Form[_])
                             (implicit request: Request[_], messages: Messages): Html = {
    val pageContext = service.getPageContext(pec, formData)
    pageContext.page match {
      case page: QuestionPage => questionView(page, pageContext, inputName, form)
      case page: InputPage => inputView(page, pageContext, inputName, form)
      case _ => errorHandler.badRequestTemplateWithProcessCode(Some(pec.processCode))
    }
  }

  private def populatedForm(ctx: PageContext, path: String): Form[_] = ctx.answer.fold(formProvider(questionName(path))) { answer =>
    formProvider(questionName(path)).bind(Map(questionName(path) -> answer))
  }

  private def questionName(path: String): String = path.reverse.takeWhile(_ != '/').reverse
}
