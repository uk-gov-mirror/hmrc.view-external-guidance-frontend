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
import forms.NextPageFormProvider
import models.ui._
import play.api.test.CSRFTokenHelper._

import scala.concurrent.Future

class GuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait QuestionTest extends MockGuidanceService {
    val path = "/some-path"
    val relativePath = path.drop(1)
    val processId = "ext90002"
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val ans1 = Answer(Text("ANS1", "ANS1"), Some(Text("","")), "/hello")
    val ans2 = Answer(Text("ANS2", "ANS2"), Some(Text("","")), "/world")
    val expectedPage: Page = QuestionPage(
      path,
      Question(Text("QUESTION","QUESTION"), Seq(Paragraph(Text("QUESTION","QUESTION"))), Seq(ans1, ans2))
    )

    MockGuidanceService
      .getPage(path, processId)
      .returns(Future.successful(Some(expectedPage)))

    val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    val view = app.injector.instanceOf[views.html.standard_page]
    val questionView = app.injector.instanceOf[views.html.question_page]
    val target = new GuidanceController(errorHandler, 
                                             view, 
                                             questionView, 
                                             new NextPageFormProvider(), 
                                             mockGuidanceService, 
                                             stubMessagesControllerComponents())
  }


  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      val result: Future[Result] = target.getPage(relativePath)(fakeRequest)
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      val result: Future[Result] = target.getPage(relativePath)(fakeRequest)
      contentType(result) mustBe Some("text/html")
    }    

  }

  "Submitting a blank Question page form" should {

    "return a BadRequest response" in new QuestionTest {
      val result: Future[Result] = target.submitPage(relativePath)(fakeRequest)
      status(result) mustBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      override val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody(("url" -> "/hello")).withCSRFToken
      val result: Future[Result] = target.submitPage(relativePath)(fakeRequest)
      status(result) mustBe Status.SEE_OTHER
    }
  }

  trait ScratchTest extends MockGuidanceService {
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    val expectedUrl = "/start-url"

    private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    private lazy val view = app.injector.instanceOf[views.html.standard_page]
    private lazy val questionView = app.injector.instanceOf[views.html.question_page]

    lazy val target = new GuidanceController(errorHandler, 
                                             view, 
                                             questionView, 
                                             new NextPageFormProvider(), 
                                             mockGuidanceService, 
                                             stubMessagesControllerComponents())
    
  }

  "Calling the scratch process endpoint with a valid UUID" should {

    trait ScratchTestWithValidUUID extends ScratchTest {
      MockGuidanceService
        .scratchProcess(uuid, uuid)
        .returns(Future.successful(Some(expectedUrl)))

      lazy val result: Future[Result] = target.scratch(uuid)(fakeRequest)
    }

    "redirect the caller to another page" in new ScratchTestWithValidUUID {
      status(result) mustBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new ScratchTestWithValidUUID {
      redirectLocation(result) mustBe Some(s"/guidance$expectedUrl")
    }

    "create a session using the standard naming" in new ScratchTestWithValidUUID {
      session(result).data.keys must contain(SessionKeys.sessionId)
    }
  }


  "Calling the scratch process endpoint with a valid UUID and existing sessionId" should {

    "Use the existing session ID" in new ScratchTest {
      MockGuidanceService
        .scratchProcess(uuid, uuid)
        .returns(Future.successful(Some(expectedUrl)))

      val result: Future[Result] = target.scratch(uuid)(fakeRequest.withSession(SessionKeys.sessionId -> "SESSIONID"))
      session(result).data.get(SessionKeys.sessionId) mustBe Some("SESSIONID")
    }
  }

  "Calling the scratch process endpoint with an invalid UUID" should {

    "return a NOT_FOUND error" in new ScratchTest {
      MockGuidanceService
        .scratchProcess(uuid, uuid)
        .returns(Future.successful(None))
      val result: Future[Result] = target.scratch(uuid)(fakeRequest)
      status(result) mustBe Status.NOT_FOUND
    }

  }

  trait StartJourneyTest extends MockGuidanceService {
    lazy val processId = "ext90002"
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    val expectedUrl = "/start-url"


    private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    private lazy val view = app.injector.instanceOf[views.html.standard_page]
    private lazy val questionView = app.injector.instanceOf[views.html.question_page]

    lazy val target = new GuidanceController(errorHandler, 
                                             view, 
                                             questionView, 
                                             new NextPageFormProvider(), 
                                             mockGuidanceService, 
                                             stubMessagesControllerComponents())
    
  }

  "Calling the start process endpoint with a valid process ID" should {

    "redirect the caller to another page" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))

      val result: Future[Result] = target.startJourney(processId)(fakeRequest)
      status(result) mustBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result: Future[Result] = target.startJourney(processId)(fakeRequest)
      redirectLocation(result) mustBe Some(s"/guidance$expectedUrl")
    }

    "add the process ID to the user's session" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result: Future[Result] = target.startJourney(processId)(fakeRequest)
      session(result).data.keys must contain(SessionKeys.sessionId)
    }
  }

  "Calling the start process endpoint with a valid process ID and existing sessionId" should {

    "Use the existing session ID" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result: Future[Result] = target.startJourney(processId)(fakeRequest.withSession(SessionKeys.sessionId -> "SESSIONID"))
      session(result).data.get(SessionKeys.sessionId) mustBe Some("SESSIONID")
    }
  }

  "Calling start process endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new StartJourneyTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .getStartPageUrl(unknownProcessId, unknownProcessId)
        .returns(Future.successful(None))
      val result: Future[Result] = target.startJourney(unknownProcessId)(fakeRequest)
      status(result) mustBe Status.NOT_FOUND
    }

  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService {
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
      private lazy val view = app.injector.instanceOf[views.html.standard_page]
      private lazy val questionView = app.injector.instanceOf[views.html.question_page]

      lazy val target = new GuidanceController(errorHandler, 
                                               view, 
                                               questionView, 
                                               new NextPageFormProvider(), 
                                               mockGuidanceService, 
                                               stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling any URL path for a page in a process with an invalid session" should {

    trait Test extends MockGuidanceService {
      val path = "some-path"
      private val pathUsedToFindPage = "/" + path
      lazy val processId = "ext90002"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()
      val expectedPage: Page = Page(
        path,
        Seq(H1(Text("hello", "Welsh: hello")))
      )

      MockGuidanceService
        .getPage(pathUsedToFindPage, processId)
        .returns(Future.successful(Some(expectedPage)))

      private lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
      private lazy val view = app.injector.instanceOf[views.html.standard_page]
      private lazy val questionView = app.injector.instanceOf[views.html.question_page]

      lazy val target = new GuidanceController(errorHandler, 
                                               view, 
                                               questionView, 
                                               new NextPageFormProvider(), 
                                               mockGuidanceService, 
                                               stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path)(fakeRequest)
    }

    "return a not found response" in new Test {
      status(result) mustBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling a valid URL path via a form submission for a page in a process" should {

    trait Test extends MockGuidanceService {
      val path = "some-path"
      val pathInQuerystring = "/guidance/some-path"
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
      private lazy val view = app.injector.instanceOf[views.html.standard_page]
      private lazy val questionView = app.injector.instanceOf[views.html.question_page]

      lazy val target = new GuidanceController(errorHandler, 
                                               view, 
                                               questionView, 
                                               new NextPageFormProvider(), 
                                               mockGuidanceService, 
                                               stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path)(fakeRequest)
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
      private lazy val view = app.injector.instanceOf[views.html.standard_page]
      private lazy val questionView = app.injector.instanceOf[views.html.question_page]

      lazy val target = new GuidanceController(errorHandler, 
                                               view, 
                                               questionView, 
                                               new NextPageFormProvider(), 
                                               mockGuidanceService, 
                                               stubMessagesControllerComponents())
      lazy val result: Future[Result] = target.getPage(path)(fakeRequest)
    }

    "return a not found response" in new Test {
      status(result) mustBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }
}
