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
import mocks.{MockAppConfig, MockGuidanceService}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{BodyParsers,AnyContentAsEmpty}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import forms.NextPageFormProvider
import models.ui._
import play.api.test.CSRFTokenHelper._
import play.api.data.FormError
import models.errors._
import models.ocelot.LabelCache
import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction

class GuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait TestData {
    val answerUrl1 = "/hello"
    val answerUrl2 = "/world"
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val path = "/some-path"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"
    lazy val processCode = "testExample"

    lazy val ans1 = Answer(Text("ANS1", "ANS1"), Some(Text("", "")), answerUrl1)
    lazy val ans2 = Answer(Text("ANS2", "ANS2"), Some(Text("", "")), answerUrl2)

    lazy val expectedPage: Page = QuestionPage(
      path,
      Question(Text("QUESTION", "QUESTION"), None, Seq(Paragraph(Text("QUESTION", "QUESTION"))), Seq(ans1, ans2))
    )

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val standardPage: Page = Page(
      standardPagePath,
      Seq(H1(Text("hello", "Welsh: hello")))
    )

    val fakeSessionIdAction = new SessionIdAction {
      def parser: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
      implicit protected def executionContext: ExecutionContext = ExecutionContext.global
      override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = block(request)
    }

    lazy val errorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view = app.injector.instanceOf[views.html.standard_page]
    lazy val questionView = app.injector.instanceOf[views.html.question_page]

  }

  trait QuestionTest extends MockGuidanceService with TestData {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      questionView,
      new NextPageFormProvider(),
      mockGuidanceService,
      stubMessagesControllerComponents()
    )
  }

  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/"), Text(Nil, Nil), processId, processCode))))

      val result = target.getPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/"), Text(Nil, Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Returning to a previously answered Question page in a process" should {

    "Show the original answer selected" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), None, Some(answerUrl1)))))

      val result = target.getPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains("checked") shouldBe true
    }
  }

  "Submitting a blank Question page form" should {

    "return a BadRequest response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, Some(FormData(relativePath, Map(), List(formError))))
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/"), Text(Nil, Nil), processId, processCode))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, Some(FormData(relativePath, Map(), List())))
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      MockGuidanceService
        .saveAnswerToQuestion(processId, path, "/guidance/hello")
        .returns(Future.successful(Right({})))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "/guidance/hello"))
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a SeeOther response whether the saving of the question succeeds or not" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, Some(FormData(relativePath, Map(), List())))
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      MockGuidanceService
        .saveAnswerToQuestion(processId, path, "/guidance/hello")
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "/guidance/hello"))
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page where url not found in process" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, "/unknown", processId, Some(FormData("unknown", Map(), List(new FormError("unknown", List("error.required"))))))
        .returns(Future.successful(Left(BadRequestError)))

      override val fakeRequest = FakeRequest("POST", "/unknown")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, "unknown")(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response if trying to submit a page which is not a question" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, standardPagePath, processId, Some(FormData(relativeStdPath, Map(), List(new FormError(relativeStdPath, List("error.required"))))))
        .returns(Future.successful(Right(PageContext(standardPage, Some("/"), Text(Nil, Nil), processId, processCode))))

      override val fakeRequest = FakeRequest("POST", standardPagePath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativeStdPath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a NOT_FOUND response if trying to submit to a non-existent page" in new QuestionTest {
      val unknownPath = "/non-existent"
      val unknownRelativePath = unknownPath.drop(1)
      MockGuidanceService
        .getPageContext(processId, unknownPath, processId, Some(FormData(unknownRelativePath, Map(), List(new FormError(unknownRelativePath, List("error.required"))))))
        .returns(Future.successful(Left(NotFoundError)))

      override val fakeRequest = FakeRequest("POST", unknownPath)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, unknownRelativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a INTERNAL_SERVER_ERROR response if submitting to a Process containing errors is referenced" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, Some(FormData(relativePath, Map(), List(formError))))
        .returns(Future.successful(Left(InvalidProcessError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a INTERNAL_SERVER_ERROR response if encountering a database error when submitting a page" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId, Some(FormData(relativePath, Map(), List(formError))))
        .returns(Future.successful(Left(DatabaseError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  trait ProcessTest extends MockGuidanceService with TestData {
    lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    lazy val target =
      new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        questionView,
        new NextPageFormProvider(),
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Right(PageContext(standardPage, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      MockGuidanceService
        .saveLabels(processId, LabelCache())
        .returns(Future.successful(Right({})))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          new NextPageFormProvider(),
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath)(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a valid URL path for a page and encountering a database error" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          new NextPageFormProvider(),
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath)(fakeRequest)
    }

    "return an INTERNAL_SERVER_ERROR response" in new Test {
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling unknown URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      val unknownPath = "/BlahBlah"
      lazy val fakeRequest = FakeRequest(GET, unknownPath).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, unknownPath, processId, None)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          new NextPageFormProvider(),
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath.drop(1))(fakeRequest)
    }

    "return a success response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling any URL path for a page in a process with an invalid session" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest()

      MockGuidanceService
        .getPageContext(processId, path, processId, None)
        .returns(Future.successful(Right(PageContext(expectedPage, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          new NextPageFormProvider(),
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, path)(fakeRequest)
    }

    "return a bad request response" in new Test {

      status(result) shouldBe Status.BAD_REQUEST
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

  "Calling a non-existing URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      val unknownPath = "unknown/route"
      lazy val fakeRequest: FakeRequest[AnyContentAsEmpty.type] =
        FakeRequest().withSession(SessionKeys.sessionId -> processId)

      MockGuidanceService
        .getPageContext(processId, "/" + unknownPath, processId, None)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          new NextPageFormProvider(),
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath)(fakeRequest)
    }

    "return not found response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }

}
