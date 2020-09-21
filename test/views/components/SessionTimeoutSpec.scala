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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements
import mocks.MockAppConfig
import views.html.session_timeout
import base.{BaseSpec, ViewFns, ViewSpec}
import play.api.test.FakeRequest

class SessionTimeoutSpec extends BaseSpec with ViewFns with ViewSpec with GuiceOneAppPerSuite {

  trait Test {

    private def injector: Injector = app.injector

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    def sessionTimeout: session_timeout = injector.instanceOf[session_timeout]

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val fakeRequest = FakeRequest("GET", "/")

    val pageTitleKey = "session.timeout.page.title"
    val pageTitleKeyAfterError = "session.timeout.after.error"
    val processTitle = "How to make a cup of tea"
    val defaultProcessTitle = "Interactive Guidance"
    val titleKey = "session.timeout.delete.your.answers"
    val titleKeyAfterError = "session.timeout.after.error"
    val processCode = "cup-of-tea"
    val startUrl = s"${MockAppConfig.baseUrl}/$processCode"
    val buttonTarget = s"$startUrl/page-1"
    val buttonTargetAfterError = MockAppConfig.defaultSignOutUrl
    val buttonTextKey = "session.timeout.button.text"
    val buttonTextKeyAfterError = "session.timeout.after.error"
  }

  "The session timeout view" should {

    "render timeout page correctly when all input arguments are defined" in new Test {

      val doc = asDocument(sessionTimeout(
        pageTitleKey,
        processTitle,
        titleKey,
        Some(processCode),
        Some(startUrl),
        buttonTarget,
        buttonTextKey)(fakeRequest, messages))

      val pageTitleElement: Element = getSingleElementByTag(doc, "title")

      pageTitleElement.text shouldBe s"${messages(pageTitleKey)} - ${messages("service.name")} - ${messages("service.govuk")}"

      val htmlHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-header__content")

      val htmlHeaderLinks: Elements = htmlHeaderDivElement.getElementsByTag("a")

      htmlHeaderLinks.size() shouldBe 1

      htmlHeaderLinks.first.text shouldBe processTitle

      val htmlHeaderLinkAttrs: Map[String, String] = elementAttrs(htmlHeaderLinks.first)

      htmlHeaderLinkAttrs.contains("href") shouldBe true
      htmlHeaderLinkAttrs("href") shouldBe startUrl

      val pageHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-!-margin-bottom-6")

      val pageHeadings: Elements = pageHeaderDivElement.getElementsByTag("h1")

      pageHeadings.size shouldBe 1

      pageHeadings.first.text shouldBe messages(titleKey)

      val startAgainLinkElement: Option[Element] = getElementById(doc, "startAgain")

      startAgainLinkElement match {
        case Some(linkElement) => {
          val attrs: Map[String, String] = elementAttrs(linkElement)

          attrs.contains("href") shouldBe true
          attrs("href") shouldBe buttonTarget

          linkElement.text shouldBe messages(buttonTextKey)
        }
        case None => fail( "Unable to locate start again link")
      }

    }

    "render timeout page correctly when processCode and startUrl are not defined" in new Test {

      val doc = asDocument(sessionTimeout(
        pageTitleKeyAfterError,
        defaultProcessTitle,
        titleKeyAfterError,
        None,
        None,
        buttonTargetAfterError,
        buttonTextKeyAfterError)(fakeRequest, messages))

      val pageTitleElement: Element = getSingleElementByTag(doc, "title")

      pageTitleElement.text shouldBe s"${messages(pageTitleKeyAfterError)} - ${messages("service.name")} - ${messages("service.govuk")}"

      val htmlHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-header__content")

      val htmlHeaderLinks: Elements = htmlHeaderDivElement.getElementsByTag("a")

      htmlHeaderLinks.size() shouldBe 1

      htmlHeaderLinks.first.text shouldBe defaultProcessTitle

      val htmlHeaderLinkAttrs: Map[String, String] = elementAttrs(htmlHeaderLinks.first)

      htmlHeaderLinkAttrs.contains("href") shouldBe true
      htmlHeaderLinkAttrs("href") shouldBe ""

      val pageHeaderDivElement: Element = getSingleElementByClass(doc, "govuk-!-margin-bottom-6")

      val pageHeadings: Elements = pageHeaderDivElement.getElementsByTag("h1")

      pageHeadings.size shouldBe 1

      pageHeadings.first.text shouldBe messages(titleKeyAfterError)

      val startAgainLinkElement: Option[Element] = getElementById(doc, "startAgain")

      startAgainLinkElement match {
        case Some(linkElement) => {
          val attrs: Map[String, String] = elementAttrs(linkElement)

          attrs.contains("href") shouldBe true
          attrs("href") shouldBe buttonTargetAfterError

          linkElement.text shouldBe messages(buttonTextKeyAfterError)
        }
        case None => fail( "Unable to locate start again link")
      }

    }


  }

}
