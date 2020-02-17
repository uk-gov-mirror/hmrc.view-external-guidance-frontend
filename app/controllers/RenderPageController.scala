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

import play.api.mvc._
import play.api.i18n.I18nSupport
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import config.{AppConfig, ErrorHandler}
import scala.concurrent.Future
import play.api.Logger
import models.ui._

@Singleton
class RenderPageController @Inject()(appConfig: AppConfig,
                                    errorHandler: ErrorHandler,
                                    mcc: MessagesControllerComponents,
                                    view: views.html.render_page) extends FrontendController(mcc) with I18nSupport {

  implicit val config: AppConfig = appConfig

  // TODO turn this into real Process/page access
  val serviceProcessPageKey = s"dummy-service!dummy-process!${DummyPage.page.urlPath}"
  val pageMap = Map(serviceProcessPageKey -> DummyPage.page)

  def renderPage(service: String, process: String, pageUrl: String): Action[AnyContent] = Action.async { implicit request =>
    Logger.info(s"""Service "$service", Process "$process", pageUrl "$pageUrl" requested""")

    Future.successful(
      pageMap.get(s"${service}!${process}!${pageUrl}")
             .map(page => Ok(view(page)))
             .getOrElse(NotFound(errorHandler.notFoundTemplate))
    )
  }
}