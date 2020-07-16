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

import play.api.Play
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api._
import play.api.mvc.Cookies
import play.api.i18n.{MessagesApi, DefaultLangsProvider}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import config.AppConfigImpl
import uk.gov.hmrc.play.language.LanguageUtils
import play.api.http.HeaderNames

class SwitchLanguageControllerSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {
  private val fakeRequest = FakeRequest("GET", "/")
  private val env = Environment.simple()
  private val configuration = Configuration.load(env)
  private val serviceConfig = new ServicesConfig(configuration, new RunMode(configuration, Mode.Dev))
  private val appConfig = new AppConfigImpl(configuration, serviceConfig)
  val langsProvider = app.injector.instanceOf[DefaultLangsProvider]
  val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  val langUtils = new LanguageUtils(langsProvider.get, configuration)(messagesApi)

  private val controller = new SwitchLanguageController(appConfig, langUtils, stubMessagesControllerComponents())

  val playCookieName = Play.langCookieName(messagesApi)

  def confirmLangCookie(cookies: Cookies, lang: String): Unit =
    cookies.get(playCookieName) match {
      case Some(cookie) => cookie.value shouldBe lang
      case None => fail(s"Missing $playCookieName cookie")
    }

  "GET /language/cy" should {
    "return 303 and set lang to cy" in {
      val result = controller.switchToLanguage("cymraeg")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "cy")
    }
  }

  "GET /language/en" should {
    "return 303 and set lang to en" in {
      val result = controller.switchToLanguage("english")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "en")
    }
  }

  "GET /language/xxx" should {
    "return 303 and fail to set lang unknown language, revert en" in {
      val result = controller.switchToLanguage("xx")(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
      confirmLangCookie(cookies(result), "en")
    }
  }

  "Lang switch" should {
    "redirect to current page if referer is valid" in {
      val result = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, s"${appConfig.baseHostUrl}/somepage")))
      redirectLocation(result) shouldBe Some(s"${appConfig.baseHostUrl}/somepage")
      
    }

    "redirect to accessibility page if referer is bogus" in {
      val result = controller.switchToLanguage("cy")(fakeRequest.withHeaders((HeaderNames.REFERER, "https://www.bbc.co.uk")))
      redirectLocation(result) shouldBe Some("/guidance/accessibility")
      
    }
  }

}
