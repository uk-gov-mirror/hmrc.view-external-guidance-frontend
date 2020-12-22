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

import base.{BaseSpec, ViewFns}
import config.ErrorHandler
import mocks.{MockAppConfig, MockGuidanceConnector, MockGuidanceService, MockSessionRepository}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.http.Status
import play.api.mvc._
import play.api.mvc.{AnyContentAsEmpty, BodyParsers}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.play.bootstrap.tools.Stubs.stubMessagesControllerComponents
import uk.gov.hmrc.http.SessionKeys
import models.{PageContext, PageEvaluationContext}
import models.ocelot.{Labels, KeyedStanza, Phrase, Process, ProcessJson, Page}
import models.ocelot.stanzas.{Question,CurrencyInput, DateInput, _}
import models.ui
import models.ui._
import repositories.ProcessContext
import play.api.test.CSRFTokenHelper._
import play.api.data.FormError
import models.errors._
import models.ocelot.LabelCache

import scala.concurrent.{ExecutionContext, Future}
import controllers.actions.SessionIdAction
import views.html._
import services._

class GuidanceControllerSpec extends BaseSpec with ViewFns with GuiceOneAppPerSuite {

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

    lazy val expectedPage: ui.Page = FormPage(
      path,
      ui.Question(Text("QUESTION", "QUESTION"), None, Seq(Paragraph(Text("QUESTION", "QUESTION"))), Seq(ans1, ans2))
    )

    val standardPagePath = "/std-page"
    val relativeStdPath = standardPagePath.drop(1)

    val standardPage: ui.Page = ui.Page(
      standardPagePath,
      Seq(H1(Text("hello", "Welsh: hello")))
    )

    val fakeSessionIdAction = new SessionIdAction {
      def parser: BodyParsers.Default = app.injector.instanceOf[BodyParsers.Default]
      implicit protected def executionContext: ExecutionContext = ExecutionContext.global
      override def invokeBlock[A](request: Request[A], block: Request[A] => Future[Result]): Future[Result] = block(request)
    }

    lazy val errorHandler: ErrorHandler = app.injector.instanceOf[config.ErrorHandler]
    lazy val view: standard_page = app.injector.instanceOf[views.html.standard_page]
    lazy val formView: form_page = app.injector.instanceOf[views.html.form_page]

    val instructionStanza: InstructionStanza = InstructionStanza(3, Seq("3"), None, false)
    val questionStanza: Question = Question(Phrase("Which?","Which?"), Seq(Phrase("yes","yes"),Phrase("no","no")), Seq("4","5"), None, false)
    val currencyInputStanza: CurrencyInput = CurrencyInput(Seq("4"),Phrase("",""), None, "PRICE", None, false)
    val dateInputStanza: DateInput = DateInput(Seq("4"),Phrase("",""), None, "Date of birth?", None, false)
    val stanzas: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", questionStanza)
                                      )
    val stanzasWithInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
                                        KeyedStanza("1", instructionStanza),
                                        KeyedStanza("3", currencyInputStanza)
                                      )
    val stanzasWithDateInput: Seq[KeyedStanza] = Seq(KeyedStanza("start", PageStanza("/start", Seq("1"), false)),
      KeyedStanza("1", instructionStanza),
      KeyedStanza("3", dateInputStanza)
    )

    val page = Page("start", "/test-page", stanzas, Seq("4","5"))
    val inputPage = Page("start", "/test-page", stanzasWithInput, Seq("4"))
    val nonQuestionPage = Page("start", "/test-page", stanzas.drop(1), Seq("3"))
    val dateInputPage = Page("start", "/test-page", stanzasWithDateInput, Seq("4"))
  }

  trait QuestionTest extends MockGuidanceService with TestData {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(page, initialLabels)
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
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
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(
          PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di)
        )))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di))))
      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Calling a valid URL with a previous page link flag to a Question page ina a process" should {

    "return an Ok response" in new QuestionTest {

      override val fakeRequest = FakeRequest("GET", s"$path?$PreviousPageLinkQuery")
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody()
        .withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = true, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), di))))

      val result = target.getPage(processId, relativePath, Some("1"))(fakeRequest)

      status(result) shouldBe Status.OK
    }

  }

  "Returning to a previously answered Question page in a process" should {

    "Show the original answer selected" in new QuestionTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, processId, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), di, answer = Some(ansIndexZero)))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains("checked") shouldBe true
    }
  }

  trait QuestionSubmissionTest extends MockSessionRepository with MockGuidanceConnector with TestData  with ProcessJson {
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val guidanceService = new GuidanceService(
      MockAppConfig,
      mockGuidanceConnector,
      mockSessionRepository,
      new PageBuilder,
      new PageRenderer,
      new UIBuilder())

    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      guidanceService,
      stubMessagesControllerComponents()
    )
    val process = prototypeJson.as[Process]
  }

  "Submitting a blank Question page form" should {


    "return a BadRequest response" in new QuestionSubmissionTest {
      MockSessionRepository
        .get(processId, s"tell-hmrc$path", previousPageByLink = false)
        .returns(Future.successful(Right(ProcessContext(process, Map(), Map(), Map(), None))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage("tell-hmrc", relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an answered Question page form" should {

    "return a SeeOther response" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a InternalServerError when saving of answer and labels fails" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((None, initialLabels))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }

    "return a SeeOther response whether the saving of the question succeeds or not" in new QuestionTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "return a BAD_REQUEST response if trying to submit a page where url not found in process" in new QuestionTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, "/unknown", previousPageByLink = false, processId)
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
            Seq.empty,
            None,
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
        .getPageEvaluationContext(processId, standardPagePath, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

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
        .getPageEvaluationContext(processId, unknownPath, previousPageByLink = false, processId)
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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

    override lazy val expectedPage: ui.Page = FormPage(
      path,
      ui.CurrencyInput(Text("Input", "Input"), Some(Text("hint", "hint")), Seq(Paragraph(Text("para", "Para"))))
    )
    val enteredValue = "12000"
    val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

    val formError = new FormError(relativePath, List("error.required"))

    val invalidDataFormError = new FormError("", List("error.required"))


    val target = new GuidanceController(
      MockAppConfig,
      fakeSessionIdAction,
      errorHandler,
      view,
      formView,
      mockGuidanceService,
      stubMessagesControllerComponents()
    )

    val initialLabels = LabelCache()
    val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(page, initialLabels)
    val pec = PageEvaluationContext(
                page,
                vStanzas,
                di,
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
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, LabelCache(), di))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      status(result) shouldBe Status.OK
    }

    "be a HTML response" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode))))
      val result = target.getPage(processId, relativePath, None)(fakeRequest)
      contentType(result) shouldBe Some("text/html")
    }
  }

  "Returning to an input page in a process" should {

    "Show the original value entered" in new InputTest {
      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di, Some(enteredValue)))))

      val result = target.getPage(processId, relativePath, None)(fakeRequest)

      status(result) shouldBe Status.OK
      contentType(result) shouldBe Some("text/html")
      // Probably not the right place to test this
      contentAsString(result).contains(enteredValue) shouldBe true
    }
  }

  "Submitting a blank Input page form" should {

    "return a BadRequest response" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "/guidance/hello", "/guidance/hello")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.BAD_REQUEST
    }
  }

  "Submitting an Input page with an invalid value" should {

    "return a BadRequest to the current page" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
                inputPage,
                vStanzas,
                di,
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

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "invalid input")
        .returns(None)

      MockGuidanceService
        .getPageContext(pec, ValueTypeError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      override val fakeRequest = FakeRequest("POST", path).withSession(SessionKeys.sessionId -> processId)
                                                          .withFormUrlEncodedBody(relativePath -> "invalid input").withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)

      status(result) shouldBe Status.BAD_REQUEST

      val doc = asDocument(contentAsString(result))

      val inputElement = doc.getElementById(s"$relativePath-0")

      elementAttrs(inputElement)("name") shouldBe relativePath

    }
  }

  "Submitting an Input page form with a value" should {

    "return a SeeOther response" in new InputTest {

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken
      val result = target.submitPage(processId, relativePath)(fakeRequest)
      status(result) shouldBe Status.SEE_OTHER
    }

    "redirect to new page with out query string after valid submission" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "/guidance/ext90002/somewhere-else"),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/another-place"),
        None
      )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken

      val result = target.submitPage(processId, relativePath)(fakeRequest)

      redirectLocation(result) shouldBe Some("/guidance/ext90002/somewhere-else")
    }

    "redirect to previously visited page with query string after valid submission" in new InputTest {
      override val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(inputPage, initialLabels)
      override val pec = PageEvaluationContext(
        inputPage,
        vStanzas,
        di,
        sessionId,
        Map("4" -> "/guidance/ext90002/somewhere-else"),
        Some("/hello"),
        Text(),
        processId,
        "hello",
        initialLabels,
        Some("/guidance/ext90002/somewhere-else"),
        None
      )

      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
        .returns(Future.successful(Right((Some("4"), LabelCache()))))

      override val fakeRequest = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> "0")
        .withCSRFToken

      val result = target.submitPage(processId, relativePath)(fakeRequest)

      redirectLocation(result) shouldBe Some("/guidance/ext90002/somewhere-else?p=1")
    }

    "return a SeeOther response whether the saving of the input succeeds or not" in new InputTest {
      MockGuidanceService
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .validateUserResponse(pec, "0")
        .returns(Some("0"))

      MockGuidanceService
        .getPageContext(pec, NoError)
        .returns(PageContext(expectedPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

      MockGuidanceService
        .submitPage(pec, path, "0", "0")
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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
        .getPageEvaluationContext(processId, path, previousPageByLink = false, processId)
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
            Seq.empty,
            None,
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
        .getPageEvaluationContext(processId, standardPagePath, previousPageByLink = false, processId)
        .returns(Future.successful(Right(pec)))

      MockGuidanceService
        .getPageContext(pec, ValueMissingError)
        .returns(PageContext(standardPage, sessionId, Some("/hello"), Text(Nil, Nil), processId, processCode, initialLabels, di))

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
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

  }

  "Calling a valid URL path for a page in a process" should {

    trait Test extends MockGuidanceService with TestData {
      lazy val fakeRequest = FakeRequest(GET, path).withSession(SessionKeys.sessionId -> processId).withCSRFToken

      MockGuidanceService
        .getPageContext(processId, path, previousPageByLink = false, processId)
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
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
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
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Left(DatabaseError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
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
        .getPageContext(processId, path, previousPageByLink = false, processId)
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
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, relativePath, None)(fakeRequest)
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
        .getPageContext(processId, unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath.drop(1), None)(fakeRequest)
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
        .getPageContext(processId, path, previousPageByLink = false, processId)
        .returns(Future.successful(Right(PageContext(expectedPage, processId, Some("/hello"), Text(Nil, Nil), processId, processCode))))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, path, None)(fakeRequest)
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
        .getPageContext(processId, "/" + unknownPath, previousPageByLink = false, processId)
        .returns(Future.successful(Left(NotFoundError)))

      lazy val target =
        new GuidanceController(
          MockAppConfig,
          fakeSessionIdAction,
          errorHandler,
          view,
          formView,
          mockGuidanceService,
          stubMessagesControllerComponents()
        )
      lazy val result = target.getPage(processId, unknownPath, None)(fakeRequest)
    }

    "return not found response" in new Test {
      status(result) shouldBe Status.NOT_FOUND
    }

    "be a HTML response" in new Test {
      contentType(result) shouldBe Some("text/html")
    }

  }


  "Date Input processing" should {
    trait DateInputTest extends MockGuidanceService with TestData {

      override lazy val expectedPage: ui.Page = FormPage(
        path,
        ui.DateInput(Text("Input", "Input"), Some(Text("hint", "hint")), Seq(Paragraph(Text("para", "Para"))))
      )
      val enteredDate = "1/1/2020"
      val fakeRequest = FakeRequest("GET", path).withSession(SessionKeys.sessionId -> processId).withFormUrlEncodedBody().withCSRFToken

      val target = new GuidanceController(
        MockAppConfig,
        fakeSessionIdAction,
        errorHandler,
        view,
        formView,
        mockGuidanceService,
        stubMessagesControllerComponents()
      )

      val initialLabels = LabelCache()
      val (vStanzas: Seq[VisualStanza], labels: Labels, di: Option[DataInput]) = new PageRenderer().renderPage(page, initialLabels)
      val pec = PageEvaluationContext(
        page,
        vStanzas,
        di,
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

    "Calling a valid URL path to an Date Input page in a process" should {

      "return an OK response" in new DateInputTest {

        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)
        status(result) shouldBe Status.OK
      }

      "be a HTML response" in new DateInputTest {
        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di))))
        val result = target.getPage(processId, relativePath, None)(fakeRequest)
        contentType(result) shouldBe Some("text/html")
      }
    }

    "Returning to a date input page in a process" should {

      "Show the original value entered" in new DateInputTest {
        MockGuidanceService
          .getPageContext(processId, path, previousPageByLink = false, processId)
          .returns(Future.successful(Right(PageContext(expectedPage, sessionId, Some("/"), Text(Nil, Nil), processId, processCode, initialLabels, di, Some(enteredDate)))))

        val result = target.getPage(processId, relativePath, None)(fakeRequest)

        status(result) shouldBe Status.OK
        contentType(result) shouldBe Some("text/html")
        // Probably not the right place to test this
//        contentAsString(result).contains(enteredValue) shouldBe true
      }
    }
  }

}
