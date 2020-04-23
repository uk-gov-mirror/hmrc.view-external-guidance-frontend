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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import forms.NextPageFormProvider
import models.ui._
import play.api.test.CSRFTokenHelper._
import play.api.data.FormError

import scala.concurrent.Future

class GuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait TestData {
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val path = "/some-path"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"
    lazy val ans1 = Answer(Text("ANS1", "ANS1"), Some(Text("", "")), "/hello")
    lazy val ans2 = Answer(Text("ANS2", "ANS2"), Some(Text("", "")), "/world")

    lazy val expectedPage: Page = QuestionPage(
      path,
      Question(Text("QUESTION", "QUESTION"), Seq(Paragraph(Text("QUESTION", "QUESTION"))), Seq(ans1, ans2))
    )

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val standardPage: Page = Page(
      standardPagePath,
      Seq(H1(Text("hello", "Welsh: hello")))
    )

    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view = app.injector.instanceOf[views.html.standard_page]
    lazy val questionView = app.injector.instanceOf[views.html.question_page]

  }

  trait QuestionTest extends MockGuidanceService with TestData {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))
    val target = new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())
  }

  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(path, processId, None)
        .returns(Future.successful(Some(PageContext(expectedPage, "/"))))

      val result = target.getPage(relativePath)(fakeRequest)
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(path, processId, None)
        .returns(Future.successful(Some(PageContext(expectedPage, "/"))))
      val result = target.getPage(relativePath)(fakeRequest)
      contentType(result) mustBe Some("text/html")
    }
  }

  "Submitting a blank Question page form" should {

    "return a BadRequest response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(path, processId, Some(FormData(relativePath, Map(), List(formError))))
        .returns(Future.successful(Some(PageContext(expectedPage, "/"))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage(relativePath)(fakeRequest)
      status(result) mustBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(path, processId, Some(FormData(relativePath, Map(), List(formError))))
        .returns(Future.successful(Some(PageContext(expectedPage, "/"))))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "/hello"))
        .withCSRFToken
      val result = target.submitPage(relativePath)(fakeRequest)
      status(result) mustBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page which is not a question" in new QuestionTest {
      MockGuidanceService
        .getPageContext(standardPagePath, processId, Some(FormData(relativeStdPath, Map(), List(new FormError(relativeStdPath, List("error.required"))))))
        .returns(Future.successful(Some(PageContext(standardPage, "/"))))

      override val fakeRequest = FakeRequest("POST", standardPagePath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(relativeStdPath)(fakeRequest)
      status(result) mustBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response if trying to submit to a non-existent page" in new QuestionTest {
      val unknownPath = "/non-existent"
      val unkwownRelativePath = unknownPath.drop(1)
      MockGuidanceService
        .getPageContext(unknownPath, processId, Some(FormData(unkwownRelativePath, Map(), List(new FormError(unkwownRelativePath, List("error.required"))))))
        .returns(Future.successful(None))

      override val fakeRequest = FakeRequest("POST", unknownPath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(unkwownRelativePath)(fakeRequest)
      status(result) mustBe Status.BAD_REQUEST
    }

  }

  trait ScratchTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())

  }

  "Calling the scratch process endpoint with a valid UUID" should {

    trait ScratchTestWithValidUUID extends ScratchTest {
      val repositoryId = "683d9aa0-2a0e-4e28-9ac8-65ce453d2731"
      MockGuidanceService
        .scratchProcess(uuid, repositoryId)
        .returns(Future.successful(Some(expectedUrl)))

      lazy val result = target.scratch(uuid)(fakeRequest)
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

      val result = target.scratch(uuid)(fakeRequest.withSession(SessionKeys.sessionId -> "SESSIONID"))
      session(result).data.get(SessionKeys.sessionId) mustBe Some("SESSIONID")
    }
  }

  "Calling the scratch process endpoint with an invalid UUID" should {

    "return a NOT_FOUND error" in new ScratchTest {
      MockGuidanceService
        .scratchProcess(uuid, uuid)
        .returns(Future.successful(None))
      val result = target.scratch(uuid)(fakeRequest)
      status(result) mustBe Status.NOT_FOUND
    }

  }

  trait StartJourneyTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())

  }

  "Calling the start process endpoint with a valid process ID" should {

    "redirect the caller to another page" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))

      val result = target.startJourney(processId)(fakeRequest)
      status(result) mustBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.startJourney(processId)(fakeRequest)
      redirectLocation(result) mustBe Some(s"/guidance$expectedUrl")
    }

    "add the process ID to the user's session" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.startJourney(processId)(fakeRequest)
      session(result).data.keys must contain(SessionKeys.sessionId)
    }
  }

  "Calling the start process endpoint with a valid process ID and existing sessionId" should {

    "Use the existing session ID" in new StartJourneyTest {
      MockGuidanceService
        .getStartPageUrl(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.startJourney(processId)(fakeRequest.withSession(SessionKeys.sessionId -> "SESSIONID"))
      session(result).data.get(SessionKeys.sessionId) mustBe Some("SESSIONID")
    }
  }

  "Calling start process endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new StartJourneyTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .getStartPageUrl(unknownProcessId, unknownProcessId)
        .returns(Future.successful(None))
      val result = target.startJourney(unknownProcessId)(fakeRequest)
      status(result) mustBe Status.NOT_FOUND
    }

  }

  trait PublishedTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())

  }

  "Calling the published endpoint with a valid process ID" should {

    "redirect the caller to another page" in new PublishedTest {
      MockGuidanceService
        .publishedProcess(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))

      val result = target.published(processId)(fakeRequest)
      status(result) mustBe Status.SEE_OTHER
    }

    "redirect the caller to the start page of the process" in new PublishedTest {
      MockGuidanceService
        .publishedProcess(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.published(processId)(fakeRequest)
      redirectLocation(result) mustBe Some(s"/guidance$expectedUrl")
    }

    "add the process ID to the user's session" in new PublishedTest {
      MockGuidanceService
        .publishedProcess(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.published(processId)(fakeRequest)
      session(result).data.keys must contain(SessionKeys.sessionId)
    }
  }

  "Calling the published endpoint with a valid process ID and existing sessionId" should {

    "Use the existing session ID" in new PublishedTest {
      MockGuidanceService
        .publishedProcess(processId, processId)
        .returns(Future.successful(Some(expectedUrl)))
      val result = target.published(processId)(fakeRequest.withSession(SessionKeys.sessionId -> "SESSIONID"))
      session(result).data.get(SessionKeys.sessionId) mustBe Some("SESSIONID")
    }
  }

  "Calling published endpoint with a invalid process ID" should {

    "return a NOT_FOUND error" in new PublishedTest {
      val unknownProcessId = "ext90077"
      MockGuidanceService
        .publishedProcess(unknownProcessId, unknownProcessId)
        .returns(Future.successful(None))
      val result = target.published(unknownProcessId)(fakeRequest)
      status(result) mustBe Status.NOT_FOUND
    }

  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(path, processId, None)
        .returns(Future.successful(Some(PageContext(standardPage, "/"))))

      lazy val target =
        new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())
      lazy val result = target.getPage(relativePath)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) mustBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling any URL path for a page in a process with an invalid session" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

      MockGuidanceService
        .getPageContext(path, processId, None)
        .returns(Future.successful(Some(PageContext(expectedPage, "/"))))

      lazy val target =
        new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())
      lazy val result = target.getPage(path)(fakeRequest)
    }

    "return a not found response" in new Test {
      status(result) mustBe Status.BAD_REQUEST
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }

  "Calling a non-existing URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      val unknownPath = "unknown/route"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      MockGuidanceService
        .getPageContext("/" + unknownPath, processId, None)
        .returns(Future.successful(None))

      lazy val target =
        new GuidanceController(errorHandler, view, questionView, new NextPageFormProvider(), mockGuidanceService, stubMessagesControllerComponents())
      lazy val result = target.getPage(unknownPath)(fakeRequest)
    }

    "return a not found response" in new Test {
      status(result) mustBe Status.BAD_REQUEST
    }

    "be a HTML response" in new Test {
      contentType(result) mustBe Some("text/html")
    }

  }
}
