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

package views.components

import config.AppConfig
import controllers.routes
import javax.inject.Inject
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.footer.FooterItem

class FooterLinks @Inject() (implicit appConfig: AppConfig) {

  def cookieLink(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.cookies")),
    Some(appConfig.cookies)
  )

  def privacyLink(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.privacy")),
    Some(appConfig.privacy)
  )

  def termsConditionsLink(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.termsConditions")),
    Some(appConfig.termsConditions)
  )

  def govukHelpLink(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.govukHelp")),
    Some(appConfig.govukHelp)
  )

  def defaultLink(implicit messages: Messages): FooterItem = FooterItem(
    Some("GOV.UK Prototype Kit v9.1.0"),
    Some("https://govuk-prototype-kit.herokuapp.com/")
  )

  def accecssibilityLink(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.accessibility")),
    Some(routes.AccessibilityStatementController.getPage().url)
  )

  def items(implicit messages: Messages): Seq[FooterItem] = Seq(
    defaultLink,
    accecssibilityLink,
    cookieLink,
    privacyLink,
    termsConditionsLink,
    govukHelpLink
  )
}
