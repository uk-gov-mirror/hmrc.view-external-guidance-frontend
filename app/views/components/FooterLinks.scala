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

  def cookieLink()(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.links.cookies.text")),
    Some(appConfig.cookies)
  )

  def privacyLink()(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.links.privacy_policy.text")),
    Some(appConfig.privacy)
  )

  def termsConditionsLink()(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.links.terms_and_conditions.text")),
    Some(appConfig.termsConditions)
  )

  def govukHelpLink()(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.links.help_page.text")),
    Some(appConfig.govukHelp)
  )

  def accecssibilityLink(startOfGuidanceUrl: Option[String] = None)(implicit messages: Messages): FooterItem = FooterItem(
    Some(messages("footer.links.accessibility.text")),
    Some(startOfGuidanceUrl.fold(routes.AccessibilityStatementController.getPage.url)
                                (routes.AccessibilityStatementController.getPageWithUrl(_).url))
  )

  def items(startUrl: Option[String])(implicit messages: Messages): Seq[FooterItem] = Seq(
    cookieLink,
    accecssibilityLink(startUrl),
    privacyLink,
    termsConditionsLink,
    govukHelpLink
  )
}
