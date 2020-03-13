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

import base.BaseSpec
import mocks.MockGuidanceService
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc.{AnyContentAsEmpty, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys

import scala.concurrent.Future

class GuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  "Calling the start process endpoint with a valid process ID" should {

    trait StartJourneyTest extends MockGuidanceService {
      lazy val processId = "ext90002"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
      val expectedUrl = "/start-url"

      MockGuidanceService
        .getStartPageUrl(processId)
        .returns(Future.successful(expectedUrl))

      private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
      private lazy val view = app.injector.instanceOf[views.html.render_page]

      lazy val target = new GuidanceController(errorHandler, view, mockGuidanceService, stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.startJourney(processId)(fakeRequest)
    }

    "redirect the caller to another page" in new StartJourneyTest {
      status(result) mustBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new StartJourneyTest {
      redirectLocation(result) mustBe Some(s"/guidance$expectedUrl")
    }

    "add the process ID to the user's session" in new StartJourneyTest {
      session(result).data must contain(SessionKeys.sessionId -> processId)
    }
  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService {
      import models.ui._

      val path = "some-path"
      private val pathUsedToFindPage = "/" + path
      lazy val processId = "ext90002"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      val expectedPage: Page = Page(
        path,
        Seq(H1(Text("hello", "Welsh: hello")))
      )

      MockGuidanceService
        .getPage(pathUsedToFindPage, processId)
        .returns(Future.successful(Some(expectedPage)))

      private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
      private lazy val view = app.injector.instanceOf[views.html.render_page]

      lazy val target = new GuidanceController(errorHandler, view, mockGuidanceService, stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path, None)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling a valid URL path via a form submission for a page in a process" should {

    trait Test extends MockGuidanceService {
      import models.ui._

      val path = "some-path"
      val pathInQuerystring = "/guidance/some-other-path"
      private val pathUsedToFindPage = pathInQuerystring.replace("/guidance", "")
      lazy val processId = "ext90002"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      val expectedPage: Page = Page(
        path,
        Seq(H1(Text("hello", "Welsh: hello")))
      )

      MockGuidanceService
        .getPage(pathUsedToFindPage, processId)
        .returns(Future.successful(Some(expectedPage)))

      private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
      private lazy val view = app.injector.instanceOf[views.html.render_page]

      lazy val target = new GuidanceController(errorHandler, view, mockGuidanceService, stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path, Some(pathInQuerystring))(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling a non-existing URL path for a page in a process" should {

    trait Test extends MockGuidanceService {
      val path = "unknown/route"
      lazy val processId = "ext90002"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      MockGuidanceService
        .getPage("/" + path, processId)
        .returns(Future.successful(None))

      private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
      private lazy val view = app.injector.instanceOf[views.html.render_page]

      lazy val target = new GuidanceController(errorHandler, view, mockGuidanceService, stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path, None)(fakeRequest)
    }

    "return a not found response" in new Test {
      status(result) mustBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }
}
