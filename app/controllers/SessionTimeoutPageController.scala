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

import javax.inject.{Inject, Singleton}
import java.time.Instant

import play.api.Logger
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request, Result, Session}
import config.{AppConfig, ErrorHandler}
import models.errors._
import models.ui.Text
import services.GuidanceService
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.http.logging.SessionId
import uk.gov.hmrc.http.SessionKeys._
import views.html.session_timeout

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@Singleton
class SessionTimeoutPageController @Inject()(
                                              appConfig: AppConfig,
                                              service: GuidanceService,
                                              errorHandler: ErrorHandler,
                                              mcc: MessagesControllerComponents,
                                              view: session_timeout)
    extends FrontendController(mcc)
    with I18nSupport {

    val logger = Logger(getClass)

    def getPage(processCode: String): Action[AnyContent] = Action.async { implicit request =>

      implicit val messages: Messages = mcc.messagesApi.preferred(request)

      val sessionId: Option[SessionId] = hc.sessionId

      sessionId match {
        case Some(id) => {
          if(hasSessionExpired(request.session)) {
            Future.successful(createYourSessionHasExpiredResponse(messages("session.timeout.header.title"), processCode))
          } else {
            manageDeleteYourAnswersRequest(id, processCode)
          }
        }
        case None => Future.successful(createYourSessionHasExpiredResponse(messages("session.timeout.header.title"), processCode))
      }
    }

  val getPageAfterError: Action[AnyContent] = Action.async { implicit request =>

    implicit val messages: Messages = mcc.messagesApi.preferred(request)

    val sessionId: Option[SessionId] = hc.sessionId

    sessionId match {
      case Some(id) => {
        service.removeSession(id.value).map {
          case Right(_) => createDeleteYourAnswersAfterErrorResponse(messages("session.timeout.header.title"))
          case Left(err) => {
            logger.error(s"Error $err occurred attempting to remove session ${sessionId.toString}")
            InternalServerError(errorHandler.internalServerErrorTemplate)}
        }
      }
      case None => {
        Future.successful(createYourSessionHasExpiredAfterErrorResponse(messages("session.timeout.header.title")))
      }
    }
  }

  def manageDeleteYourAnswersRequest(sessionId: SessionId, processCode: String)(implicit request: Request[_]): Future[Result] = {

    // Refresh cache to remove answers
    implicit val messages: Messages = mcc.messagesApi.preferred(request)

    service.getProcessContext(sessionId.value).flatMap {
      case Right(processContext) if (processCode != processContext.process.meta.processCode) => {
        logger.error(s"Unexpected process code encountered when removing session after timeout warning. Expected code $processCode; actual code ${processContext.process.meta.processCode}")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplate))
      }
      case Right(processContext) => {
        service.removeSession(sessionId.value).map {
            case Left(err) => {
              logger.error(s"Error $err occurred attempting to remove session ${sessionId.toString}")
              InternalServerError(errorHandler.internalServerErrorTemplateWithProcessCode(Some(processCode)))
            }
            case Right(_) => {
              val title = Text(processContext.process.title.langs)
              createDeleteYourAnswersResponse(title.asString(messages.lang), processCode)
            }
        }
      }
      case Left(NotFoundError) => {
        // For some reason the timeout mechanism invokes the "getPage" method twice. In this case
        // a not found error will be returned on the second invocation because the session data will have been deleted
        Future.successful(createDeleteYourAnswersResponse(messages("session.timeout.header.title"), processCode))
      }
      case Left(err) => {
        logger.error(s"Error $err occurred retrieving process context for process $processCode when removing session")
        Future.successful(InternalServerError(errorHandler.internalServerErrorTemplateWithProcessCode(Some(processCode))))
      }
    }

  }

  def createDeleteYourAnswersResponse(processTitle: String, processCode: String)(implicit request: Request[_]): Result =
    Ok(view("session.timeout.page.title",
      processTitle,
      "session.timeout.delete.your.answers",
      Some(processCode),
      None,
      s"${appConfig.baseUrl}/$processCode",
      "session.timeout.button.text"))

  def createYourSessionHasExpiredResponse(processTitle: String, processCode: String)(implicit request: Request[_]): Result =
    Ok(view("session.timeout.page.title",
      processTitle,
      "session.timeout.session.has.expired",
      Some(processCode),
      None,
      s"${appConfig.baseUrl}/$processCode",
      "session.timeout.button.text"))

  def createDeleteYourAnswersAfterErrorResponse(processTitle: String)(implicit request: Request[_]): Result =
    Ok(view("session.timeout.page.title",
      processTitle,
      "session.timeout.delete.answers.after.error",
      None,
      None,
      appConfig.defaultSignOutUrl,
      "session.timeout.after.error.button.text"))

  def createYourSessionHasExpiredAfterErrorResponse(processTitle: String)(implicit request: Request[_]): Result =
    Ok(view("session.timeout.page.title",
      processTitle,
      "session.timeout.after.error",
      None,
      None,
      appConfig.defaultSignOutUrl,
      "session.timeout.after.error.button.text"))

  /**
    * If last request update is available check if session has timed out
    *
    * @param session - Browser session
    * @return Returns "true" if time since last update exceeds timeout limit or is very close to the limit
    */
  def hasSessionExpired(session: Session): Boolean = {

    session.get(lastRequestTimestamp).fold(false){tsStr =>

      val now = Instant.now.toEpochMilli
      val ts = tsStr.toLong

      val duration = now - ts
      val timeout = appConfig.timeoutInSeconds * appConfig.toMilliSeconds
      val diff = duration - timeout

      if((duration > timeout) || (diff.abs < appConfig.expiryErrorMarginInMilliSeconds)) true else false
    }
  }

}
