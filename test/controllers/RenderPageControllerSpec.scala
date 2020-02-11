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

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import play.api.{Configuration, Environment, _}
import uk.gov.hmrc.play.bootstrap.config.{RunMode, ServicesConfig}
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import config.{AppConfig, ErrorHandler}
import play.api.i18n.MessagesApi
import play.api.inject.Injector

class RenderPageControllerSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def injector: Injector = app.injector
  def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

  private val fakeRequest = FakeRequest("GET", "/")

  private val env           = Environment.simple()
  private val configuration = Configuration.load(env)

  private val serviceConfig = new ServicesConfig(configuration, new RunMode(configuration, Mode.Dev))
  private val appConfig     = new AppConfig(configuration, serviceConfig)
  private val controller = new RenderPageController(appConfig,
                                                    new ErrorHandler(messagesApi, appConfig),
                                                    stubMessagesControllerComponents())



  "GET /dummy-service/dummy-process/dummy-path" should {
    "return 200" in {
      val result = controller.renderPage("dummy-service","dummy-process","dummy-path")(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "return HTML" in {
      val result = controller.renderPage("dummy-service","dummy-process","dummy-path")(fakeRequest)
      contentType(result) shouldBe Some("text/html")
      charset(result)     shouldBe Some("utf-8")
    }

  }

  "GET /dummy-service/dummy-process/unknown" should {
    "return NOT_FOUND" in {
      val result = controller.renderPage("dummy-service","dummy-process","unknown")(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }
  }

}
