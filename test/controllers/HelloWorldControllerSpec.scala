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
import config.AppConfig

class HelloWorldControllerSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  private trait Test {
    val fakeRequest = FakeRequest("GET", "/")

    private val env = Environment.simple()
    private val configuration = Configuration.load(env)

    private val serviceConfig = new ServicesConfig(configuration, new RunMode(configuration, Mode.Dev))
    private val appConfig = new AppConfig(configuration, serviceConfig)

    private val view = app.injector.instanceOf[views.html.hello_world]
    val controller = new HelloWorldController(appConfig, stubMessagesControllerComponents(), view)
  }

  "GET /hello-world" should {
    "return 200" in new Test {
      val result = controller.helloWorld(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "return HTML" in new Test {
      val result = controller.helloWorld(fakeRequest)
      contentType(result) shouldBe Some("text/html")
      charset(result)     shouldBe Some("utf-8")
    }

  }

  "GET /bye-world" should {
    "throw an exception" in new Test {
      assertThrows[Exception] {

        val result = controller.byeWorld(fakeRequest)

        status( result ) shouldBe Status.OK
      }
    }
  }

}
