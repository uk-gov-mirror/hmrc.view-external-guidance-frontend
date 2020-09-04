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
import config.AppConfig
import play.api.i18n.Messages
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.Logger
import repositories.ProcessContext
import services.GuidanceService
import config.ErrorHandler
import models.errors.BadRequestError

@Singleton
class AccessibilityStatementController @Inject() (
  appConfig: AppConfig,
  mcc: MessagesControllerComponents,
  view: views.html.accessibility_statement,
  service: GuidanceService,
  errorHandler: ErrorHandler
) extends FrontendController(mcc)
  with SessionFrontendController {

  implicit val config: AppConfig = appConfig
  val logger: Logger = Logger(getClass)

  val getPage: Action[AnyContent] = Action.async { implicit request =>
    implicit val messages: Messages = mcc.messagesApi.preferred(request)
    withExistingSession[ProcessContext](service.getProcessContext(_)).map {
      case Right(processContext) =>
        val title = models.ui.Text(processContext.process.title.langs)
        val processIdentifier = processContext.process.meta.processCode.getOrElse(processContext.process.meta.id)
        Ok(view(title.asString(messages.lang),
                processContext.process.startUrl.map(url => s"${appConfig.baseUrl}/$processIdentifier$url")))
      case Left(BadRequestError) =>
        logger.warn(s"Accessibility used out of guidance")
        Ok(view(messages("accessibility.defaultHeaderTitle"), None))
      case Left(err) =>
        logger.error(s"Request for ProcessContext returned $err, returning InternalServerError")
        InternalServerError(errorHandler.internalServerErrorTemplate)
    }
  }
}
