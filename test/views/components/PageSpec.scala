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

package views.components

import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html.standard_page
import views.html.question_page
import models.PageContext
import models.ui.{BulletPointList, H1, Page, Paragraph, StandardPage, Text, Question, Answer, QuestionPage}
import org.jsoup.nodes.{Element, Document}
import forms.SubmittedAnswerFormProvider
import scala.collection.JavaConverters._
import play.api.data.FormError
import models.ui.ErrorMsg
import base.ViewFns

class PageSpec extends WordSpec with Matchers with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val standardPageView = app.injector.instanceOf[views.html.standard_page]
    val questionPageView = app.injector.instanceOf[views.html.question_page]
    val title = Text("Telling HMRC about extra income", "Tudalen Arddangos Yn Adrodd HMRC am incwm ychwanegol")

    val openingPara = Text(
      "Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.",
      "Gwiriwch a oes angen i chi ddweud wrth HMRC am arian ychwanegol rydych chi " +
        "wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo."
    )

    val bulletPointLeadingText = Text("For example:", "Er enghraifft:")

    val bulletPointOne = Text("selling items online or face to face", "gwerthu eitemau ar-lein neu wyneb yn wyneb")

    val bulletPointTwo =
      Text("selling freelance services (such as gardening or babysitting)", "gwerthu gwasanaethau ar eu liwt eu hunain (fel garddio neu warchod plant)")
    val bulletPointThree = Text("hiring out personal equipment (such as power tools)", "llogi offer personol (fel offer pŵer)")

    val para = Paragraph(openingPara)
    val bulletPointList = BulletPointList(bulletPointLeadingText, Seq(bulletPointOne, bulletPointTwo, bulletPointThree))

    val simplePage = StandardPage("root", Seq(para, H1(title), bulletPointList))

    val q1 = Vector("Do you agree?", "Welsh, Do you agree?")
    val ans1 = Vector("Yes", "Welsh, Yes")
    val ans2 = Vector("No", "Welsh, Yes")
    val a1 = Answer(Text(ans1), None, "/blah")
    val a2 = Answer(Text(ans2), None, "/other")
    val answers = Seq(a1, a2)
    val questionText = Text(q1)
    val question = Question(questionText, None, Seq(para, bulletPointList), answers)
    val errorMsg = ErrorMsg("id", Text("An error has occurred", "Welsh, An error has occurred"))
    val questionWithErrors = Question(questionText, None, Seq(para, bulletPointList), answers, Seq(errorMsg))
    val formProvider = new SubmittedAnswerFormProvider()
    val questionPage = QuestionPage("root", question)
    val questionPageWithErrors = QuestionPage("root", questionWithErrors)

    def expectedTitleText(h1Text: String, section: Option[String] = None): String =
      section.fold(s"${h1Text} - ${messages("service.name")} - ${messages("service.govuk")}"){s =>
        s"${h1Text} - ${s} - ${messages("service.name")} - ${messages("service.govuk")}"
      }

    def checkTitle(doc: Document, section: Option[String] = None, prefix: Option[String] = None): Unit =
      Option(doc.getElementsByTag("h1").first).fold(fail("Missing H1")){ h1 =>
        Option(doc.getElementsByTag("title").first).fold(fail("Missing title")){title =>
          prefix.fold(title.text shouldBe expectedTitleText(h1.text, section)){ prefx =>
            title.text shouldBe s"$prefx ${expectedTitleText(h1.text, section)}"
          }
        }
      }

    val pageContext = PageContext(simplePage, "sessionId", Some("/"), Text("Title", "Title"), "processId", "processCode")
    val questionPageContext = PageContext(questionPage, "sessionId", Some("/here"), Text("Title", "Title"), "processId", "processCode")
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Standard Page component" should {

    "generate English html containing an H1, a text only paragraph and a test only bullet point list" in new Test {
      val doc = asDocument(standardPageView(simplePage, pageContext)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.english.head.toString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.value(messages.lang).head.toString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.english.head.toString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.english.head.toString, bulletPointTwo.english.head.toString, bulletPointThree.english.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Welsh html containing an H1 and a text only paragraph" in new WelshTest {

      val doc = asDocument(standardPageView(simplePage, pageContext)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.welsh.head.toString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.welsh.head.toString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.welsh.head.toString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] = List(bulletPointOne.welsh.head.toString, bulletPointTwo.welsh.head.toString, bulletPointThree.welsh.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

  }

  "Question Page component" should {

    "generate English html containing an H1, a text only paragraph and a text only bullet point list" in new Test {

      val doc = asDocument(questionPageView(questionPage, pageContext, "question", formProvider("url") )(fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe questionText.english.head.toString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.value(messages.lang).head.toString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.english.head.toString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.english.head.toString, bulletPointTwo.english.head.toString, bulletPointThree.english.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Englsh title prefixed by Error: when errors are displayed" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(questionPageView(questionPageWithErrors, questionPageContextWithErrs, "question", formProvider("url") )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set radios fieldset aria-describedby correctly whn error occurs" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(questionPageView(questionPageWithErrors, questionPageContextWithErrs, "question", formProvider("url") )(fakeRequest, messages))

      val fieldset: Element = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){fset =>
        elementAttrs(fset)("aria-describedby").contains("id-error") shouldBe true
      }
    }

    "generate Welsh html containing an H1 and a text only paragraph" in new WelshTest {

      val doc = asDocument(questionPageView(questionPage, questionPageContext, "question", formProvider("url") )(fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe questionText.welsh.head.toString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.welsh.head.toString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.welsh.head.toString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] = List(bulletPointOne.welsh.head.toString, bulletPointTwo.welsh.head.toString, bulletPointThree.welsh.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Welsh title prefixed by Error: when errors are displayed" in new WelshTest {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(questionPageView(questionPageWithErrors, questionPageContextWithErrs, "question", formProvider("url") )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set radios fieldset aria-describedby correctly when error occurs" in new WelshTest {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(questionPageView(questionPageWithErrors, questionPageContextWithErrs, "question", formProvider("url") )(fakeRequest, messages))

      val fieldset: Element = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){fset =>
        elementAttrs(fset)("aria-describedby").contains("id-error") shouldBe true
      }
    }

  }

}
