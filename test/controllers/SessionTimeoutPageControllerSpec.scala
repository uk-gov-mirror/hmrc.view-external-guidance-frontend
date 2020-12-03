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

import java.time.Instant

import base.BaseSpec
import mocks.{MockAppConfig, MockGuidanceService}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.test.FakeRequest
import play.api.test.Helpers._
import config.ErrorHandler
import models.errors._
import models.ocelot.{Process, ProcessJson}
import repositories.ProcessContext
import views.html.{delete_your_answers, session_timeout}
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import play.api.mvc.Result

import scala.concurrent.Future

class SessionTimeoutPageControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  private trait Test extends MockGuidanceService with ProcessJson {

    lazy val errorHandler = app.injector.instanceOf[ErrorHandler]
    lazy val view = app.injector.instanceOf[session_timeout]
    lazy val delete_answers_view = app.injector.instanceOf[delete_your_answers]

    lazy val processCode = "cup-of-tea"
    lazy val sessionId = "sessionId"
    lazy val invalidProcessCode = "cup-of-coffee"

    lazy val process: Process = validOnePageJson.as[Process]
    lazy val processContext: ProcessContext = ProcessContext(process, Map(), Map(), Map(), None)

    val timeout = MockAppConfig.timeoutInSeconds * MockAppConfig.toMilliSeconds

    val target = new SessionTimeoutPageController( MockAppConfig,
      mockGuidanceService,
      errorHandler,
      stubMessagesControllerComponents(),
      view,
      delete_answers_view)

  }

  "SessionTimeoutPageController method getPage invoked by delete your answers link" should {

    "delete the current session data if it exists" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getProcessContext(sessionId).returns(Future.successful(Right(processContext)))

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }


    "return an internal server error if a database error occurs retrieving the session data" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getProcessContext(sessionId).returns(Future.successful(Left(DatabaseError)))

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return an internal server error if the process code in the session data does not match value of input argument" in new Test {

      val now: String = Instant.now.toEpochMilli.toString

      val fakeRequest = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> now)

      MockGuidanceService.getProcessContext(sessionId).returns(Future.successful(Right(processContext)))

      val result: Future[Result] = target.getPage(invalidProcessCode)(fakeRequest)

      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  "SessionTimeoutPageController method getPage invoked after timeout dialog expires" should {

    "return a successful response if the session has expired" in new Test {

      val fakeRequest = FakeRequest("GET", "/")

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return successful response when session remains but session timeout exceeded by 60 seconds " in new Test {

      // This scenario should not occur but is catered for as a possible extension of the timeout expiry calculation
      val now: Long = Instant.now.toEpochMilli
      val ts = now - (timeout + (60 * MockAppConfig.toMilliSeconds))

      val fakeRequest = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> ts.toString
      )

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }

    "return successful response when method is triggered just before session should have timed out" in new Test {

      // This scenario occurs when the timeout dialog disappears fractionally before the session times out
      val now: Long = Instant.now.toEpochMilli
      val ts = now - (timeout - 10)

      val fakeRequest = FakeRequest("GET", "/").withSession(
        SessionKeys.sessionId -> sessionId,
        SessionKeys.lastRequestTimestamp -> ts.toString
      )

      val result: Future[Result] = target.getPage(processCode)(fakeRequest)

      status(result) shouldBe Status.OK
    }
  }

}
