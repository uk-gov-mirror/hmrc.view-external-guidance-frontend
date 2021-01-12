/*
 * Copyright 2021 HM Revenue & Customs
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

package mocks

import config.AppConfig
import play.api.Configuration
import play.api.i18n.Lang
import play.api.mvc.RequestHeader

import scala.collection.immutable.ListMap

object MockAppConfig extends AppConfig {
  val assetsPrefix: String = "someUrl"
  val languageMap: Map[String, Lang] = ListMap("english" -> Lang("en"), "cymraeg" -> Lang("cy"))
  val config: Configuration = Configuration()
  override val analyticsToken: String = "token"
  override val analyticsHost: String = "host"
  override val reportAProblemPartialUrl: String = "someUrl"
  override val reportAProblemNonJSUrl: String = "someJsUrl"
  override val externalGuidanceBaseUrl: String = "http://external-guidance-base-url"
  override val cookies: String = "someUrl"
  override val privacy: String = "someUrl"
  override val termsConditions: String = "someUrl"
  override val govukHelp: String = "someUrl"
  override val accessibilityStatement: String = "someUrl"
  override val signOutUrl: String = "/guidance/sessionTimeout"
  override val defaultSignOutUrl: String = "https://www.gov.uk"
  override val timeoutInSeconds: Int = 1200
  override val timeoutWarningInSeconds: Int = 300
  override val toMilliSeconds: Int = 1000
  override val expiryErrorMarginInMilliSeconds: Int = 100
  override def feedbackUrl(implicit request: RequestHeader): String = "somefeedbackUrl"
  override val baseUrl: String = "/guidance"
  override val host: String = "http://localhost:9741"
  override val adminHost: String = "http://adminhost"
  override val hostBaseUrl: String = s"${host}${baseUrl}"
  override val adminHostBaseUrl: String = s"${adminHost}${baseUrl}"
}
