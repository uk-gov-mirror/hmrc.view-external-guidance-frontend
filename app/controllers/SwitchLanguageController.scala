/*
 * Copyright 2019 HM Revenue & Customs
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

import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import play.api.i18n.Lang
import play.api.Configuration
import config.AppConfig
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}

@Singleton
class SwitchLanguageController @Inject()(appConfig: AppConfig,
                                         languageUtils: LanguageUtils,
                                         cc: MessagesControllerComponents) extends LanguageController(appConfig.config, languageUtils, cc) {

  override def languageMap: Map[String, Lang] = appConfig.languageMap

  // TODO requires suitable index like fallback URL
  override def fallbackURL: String = routes.HelloWorldController.helloWorld().url
}
