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
import mocks.{MockAppConfig, MockGuidanceService, MockSessionRepository, MockGuidanceConnector}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{AnyContentAsEmpty, BodyParsers}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import forms.SubmittedAnswerFormProvider
import models.{PageEvaluationContext, PageContext}
import models.ocelot.{Process, ProcessJson, Phrase,KeyedStanza, Page => OcelotPage}
import models.ocelot.stanzas.{Question => OcelotQuestion,_}
import models.ui.{CurrencyInput => UiCurrencyInput,_}
import repositories.ProcessContext
import play.api.test.CSRFTokenHelper._
import play.api.data.FormError
import models.errors._
import models.ocelot.LabelCache
import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction
import views.html._
import services._

class GuidanceControllerSpec extends BaseSpec with GuiceOneAppPerSuite {

  trait TestData {
    val ansIndexZero = "0"
    lazy val uuid = "683d9aa0-2a0e-4e28-9ac8-65ce453d2730"
    lazy val sessionId = "session-2882605c-8e96-494a-a497-98ae90f52539"
    lazy val path = "/some-path"
    lazy val relativePath = path.drop(1)
    lazy val expectedUrl = "/start-url"
    lazy val processId = "ext90002"
    lazy val processCode = "testExample"

    lazy val ans1 = Answer(Text("ANS1", "ANS1"), Some(Text("", "")))
    lazy val ans2 = Answer(Text("ANS2", "ANS2"), Some(Text("", "")))

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
    lazy val inputView = app.injector.instanceOf[input_page]
    val instructionStanza = InstructionStanza(3, Seq("3"), None, false)
    val questionStanza = OcelotQuestion(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", questionStanza)
                                      )
    val page = OcelotPage("start", "/test-page", stanzas, Seq("4","5"))

    val nonQuestionPage = OcelotPage("start", "/test-page", stanzas.drop(1), Seq("3"))
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
      inputView,
      new SubmittedAnswerFormProvider(),
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()

    val pec = PageEvaluationContext(
                page,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )
  }

  "Calling a valid URL path to a Question page in a process" should {

    "return an OK response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode))))

      val result = target.getPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Returning to a previously answered Question page in a process" should {

    "Show the original answer selected" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, processId, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), None, Some(ansIndexZero)))))

      val result = target.getPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains("checked") shouldBe true
    }
  }

  trait QuestionSubmissionTest extends MockSessionRepository with MockGuidanceConnector with TestData  with ProcessJson {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val guidanceService = new GuidanceService(MockAppConfig, mockGuidanceConnector, mockSessionRepository, new PageBuilder, new PageRenderer, new UIBuilder)

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      questionView,
      inputView,
      new SubmittedAnswerFormProvider(),
      guidanceService,
      stubMessagesControllerComponents()
    )
    val process = prototypeJson.as[Process]
  }

  "Submitting a blank Question page form" should {


    "return a BadRequest response" in new QuestionSubmissionTest {
      MockSessionRepository
        .get(processId, s"tell-hmrc$path")
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), None))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage("tell-hmrc", relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List())))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "0"))
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a InternalServerError when saving of answer and labels fails" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List())))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST when submitting page and guidance determines invalid data" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, None)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Right((None, initialLabels))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "0"))
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a SeeOther response whether the saving of the question succeeds or not" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List())))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody((relativePath -> "0"))
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page where url not found in process" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, "/unknown", processId)
        .returns(Future.successful(Left(BadRequestError)))

      override val fakeRequest = FakeRequest("POST", "/unknown")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, "unknown")(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response if trying to submit a page which is not a question" in new QuestionTest {
      override val pec = PageEvaluationContext(
            nonQuestionPage,
            sessionId,
            Map(),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            LabelCache(),
            None,
            None
          )

      MockGuidanceService
        .getPageEvaluationContext(processId, standardPagePath, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativeStdPath, Map(), List(new FormError(relativeStdPath, List("error.required"))))))
        .returns(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

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
        .getPageEvaluationContext(processId, unknownPath, processId)
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
        .getPageEvaluationContext(processId, path, processId)
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
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left(DatabaseError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

  }

  trait InputTest extends MockGuidanceService with TestData {

    override lazy val expectedPage: Page = InputPage(
      path,
      UiCurrencyInput(Text("Input", "Input"), Some(Text("hint", "hint")), Seq(Paragraph(Text("para", "Para"))))
    )
    val enteredValue = "12000"
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      questionView,
      inputView,
      new SubmittedAnswerFormProvider(),
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()

    val pec = PageEvaluationContext(
                page,
                sessionId,
                Map("4" -> "/somewhere-else"),
                Some("/hello"),
                Text(),
                processId,
                "hello",
                initialLabels,
                None,
                None
              )
  }

  "Calling a valid URL path to an Input page in a process" should {

    "return an OK response" in new InputTest {

      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode))))

      val result = target.getPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Returning to an input page in a process" should {

    "Show the original value entered" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), None, Some(enteredValue)))))

      val result = target.getPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains(enteredValue) shouldBe true
    }
  }

  "Submitting a blank Input page form" should {

    "return a BadRequest response" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List(formError))))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "/guidance/hello")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an Input page form with a value" should {

    "return a SeeOther response" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List(formError))))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a SeeOther response whether the saving of the input succeeds or not" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativePath, Map(), List(formError))))
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      MockGuidanceService
        .submitPage(pec, path, "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a INTERNAL_SERVER_ERROR response if submitting to a Process containing errors is referenced" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left(InvalidProcessError)))
      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a NOT_FOUND response if trying to submit to a non-existent page" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left(NotFoundError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.NOT_FOUND
    }

    "return a InternalServerError response when an unexpected error returned from service call" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left(DatabaseError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.INTERNAL_SERVER_ERROR
    }

    "return a BAD_REQUEST response when a bad request error returned from service call" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, processId)
        .returns(Future.successful(Left(BadRequestError)))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "/guidance/hello")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a BAD_REQUEST response when submitted page is not an input or question" in new QuestionTest {

      override val pec = PageEvaluationContext(
            nonQuestionPage,
            sessionId,
            Map(),
            Some("/hello"),
            Text(),
            processId,
            "hello",
            LabelCache(),
            None,
            None
          )

      MockGuidanceService
        .getPageEvaluationContext(processId, standardPagePath, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, Some(FormData(relativeStdPath, Map(), List(new FormError(relativeStdPath, List("error.required"))))))
        .returns(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken
      val result = target.submitPage(processId, relativeStdPath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
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
        inputView,
        new SubmittedAnswerFormProvider(),
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      MockGuidanceService
        .saveLabels(sessionId, LabelCache())
        .returns(Future.successful(Right({})))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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

  "Calling a valid URL path for a page and encountering a database error when saving labels" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      MockGuidanceService
        .saveLabels(sessionId, LabelCache())
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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
        .getPageContext(processId, unknownPath, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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
        .getPageContext(processId, path, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, processId, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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
        .getPageContext(processId, "/" + unknownPath, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          questionView,
          inputView,
          new SubmittedAnswerFormProvider(),
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
