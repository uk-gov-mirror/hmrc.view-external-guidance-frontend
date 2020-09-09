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

import com.google.inject.{Inject, Singleton}
import play.api.mvc._
import play.api.i18n.Lang
import config.AppConfig
import play.api.Logger
import uk.gov.hmrc.play.language.{LanguageController, LanguageUtils}

@Singleton
class SwitchLanguageController @Inject() (appConfig: AppConfig, languageUtils: LanguageUtils, cc: MessagesControllerComponents)
    extends LanguageController(appConfig.config, languageUtils, cc) {

  val logger = Logger(getClass)

  override def languageMap: Map[String, Lang] = appConfig.languageMap
  private val SwitchIndicatorKey = "switching-language"
  private val FlashWithSwitchIndicator = Flash(Map(SwitchIndicatorKey -> "true"))
  private val referrerPattern = "(https?:?\\/\\/[a-zA-Z0-9\\.\\-_:]+)(\\/.+)?".r
  override def fallbackURL: String = routes.AccessibilityStatementController.getPage().url

  override def switchToLanguage(language: String): Action[AnyContent] = Action { implicit request =>
    val enabled: Boolean = languageMap.get(language).exists(languageUtils.isLangAvailable)
    val lang: Lang =
      if (enabled) languageMap.getOrElse(language, languageUtils.getCurrentLang)
      else languageUtils.getCurrentLang

    // If there is no referrer, redirect to the fallback getPage
    // Match on referrer made up of an http(s)://host and a path relative to this
    // If http(s)://host exists and it starts with either the service host or
    // admin host, this will stripped off the remaining url will be returned
    // If the http(s)://host doesnt match, the fallback url is returned
    // If no http(s)://host the relative path is returned unless it is empty
    // in which case the fallback url is returned
    val relativeUrl: String = request.headers.get(REFERER).fold(fallbackURL){r =>
      referrerPattern.findFirstMatchIn(r).fold(r){m =>
        Option(m.group(1)).fold(Option(m.group(2)).fold(fallbackURL)(x => x)){ host =>
          if (host.startsWith(appConfig.host) || host.startsWith(appConfig.adminHost)) Option(m.group(2)).fold(fallbackURL)(x => x) else fallbackURL
        }
      }
    }
    logger.info(s"Redirecting to $relativeUrl")
    Redirect(relativeUrl).withLang(Lang.apply(lang.code)).flashing(FlashWithSwitchIndicator)
  }
}
