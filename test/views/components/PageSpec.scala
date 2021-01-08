/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.data.Forms.nonEmptyText
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html.standard_page
import views.html.form_page
import models.PageContext
import models.ui.{Answer, BulletPointList, ConfirmationPanel, CurrencyInput, RequiredErrorMsg, H1, Input, FormPage, InsetText}
import models.ui.{NumberedCircleList, NumberedList, Page, Paragraph, Question, FormPage, StandardPage, CyaSummaryList, Text}
import org.jsoup.nodes.{Document, Element}

import scala.collection.JavaConverters._
import play.api.data.FormError
import base.ViewFns
import forms.SubmittedTextAnswerFormProvider

class PageSpec extends WordSpec with Matchers with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val standardPageView = app.injector.instanceOf[views.html.standard_page]
    val formPageView = app.injector.instanceOf[views.html.form_page]
    val title = Text("Telling HMRC about extra income")

    val openingPara = Text("Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.")

    val bulletPointLeadingText = Text("For example:")

    val bulletPointOne = Text("selling items online or face to face")

    val bulletPointTwo =
      Text("selling freelance services (such as gardening or babysitting)")
    val bulletPointThree = Text("hiring out personal equipment (such as power tools)")

    val para = Paragraph(openingPara)
    val bulletPointList = BulletPointList(bulletPointLeadingText, Seq(bulletPointOne, bulletPointTwo, bulletPointThree))
    val simplePage = StandardPage("root", Seq(para, H1(title), bulletPointList))

    val confirmationPanelLeadingText = Text("Calculation Complete")
    val confirmationPanelOne = Text("you need to pay IHT")
    val confirmationPanelTwo = Text("£325,000")
    val confirmationPanel = ConfirmationPanel(confirmationPanelLeadingText, Seq(confirmationPanelOne, confirmationPanelTwo))
    val listOne = Text("Line 1")
    val listTwo = Text("Line 2")
    val listThree = Text("Line 2")
    val numberedList = NumberedList(Seq(listOne, listTwo, listThree))
    val numberedCircleList = NumberedCircleList(Seq(listOne, listTwo, listThree))
    val insetOne = Text("Inset 1")
    val insetTwo = Text("Inset 2")
    val insetText = InsetText(Seq(insetOne, insetTwo))
    val summaryList = CyaSummaryList(Seq(Seq(listOne, listOne), Seq(listTwo, listThree)))
    val outcomePage = StandardPage("root", Seq(confirmationPanel, numberedList, insetText, numberedCircleList, summaryList))

    val a1 = Answer(Text("Yes"), None)
    val a2 = Answer(Text("No"), None)
    val answers = Seq(a1, a2)
    val questionText = Text("Do you agree?")
    val question = Question(questionText, None, Seq(para, bulletPointList), answers)
    val errorMsg = RequiredErrorMsg(Text("An error has occurred"))
    val questionWithErrors = Question(questionText, None, Seq(para, bulletPointList), answers, Seq(errorMsg))
    val textFormProvider = new SubmittedTextAnswerFormProvider()
    val questionPage = FormPage("root", question)
    val questionPageWithErrors = FormPage("root", questionWithErrors)

    val inputText = Text("What is value of your house?")
    val inputHint = Text("use market value")
    val input = CurrencyInput(inputText, Some(inputHint), Seq(para, bulletPointList))
    val inputWithErrors = CurrencyInput(inputText, Some(inputHint), Seq(para, bulletPointList), Seq(errorMsg))
    val inputPage = FormPage("root", input)
    val inputPageWithErrors = FormPage("root", inputWithErrors)

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

    val pageCtx = PageContext(simplePage, Seq.empty, None, "sessionId", Some("/"), Text("Title"), "processId", "processCode")
    val questionPageContext = PageContext(questionPage, Seq.empty, None, "sessionId", Some("/here"), Text("Title"), "processId", "processCode")
    val inputPageContext = PageContext(inputPage, Seq.empty, None, "sessionId", Some("/here"), Text("Title"), "processId", "processCode")
  }

  "Standard Page component" should {

    "generate English html containing an H1, a text only paragraph and a test only bullet point list" in new Test {
      val doc = asDocument(standardPageView(simplePage, pageCtx)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] = List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate English html containing a confirmation panel, an inset text and a numbered list" in new Test {
      val doc = asDocument(standardPageView(outcomePage, pageCtx)(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe confirmationPanelLeadingText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val div = doc.getElementsByClass("govuk-inset-text")
      div.size shouldBe 1

      val insetInfo = div.first().getElementsByTag("p")
      insetInfo.size shouldBe 2

      val firstPara = insetInfo.eq(0)
      firstPara.first.text shouldBe insetOne.asString

      val secondPara = insetInfo.eq(1)
      secondPara.first.text shouldBe insetTwo.asString

      val numberedListItem = doc.getElementsByClass("govuk-list--number")
      numberedListItem.size shouldBe 2

      val actualListItems = numberedListItem.first().getElementsByTag("li").asScala.toList

      val expectedListItems: List[String] =
        List(listOne.asString, listTwo.asString, listThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual numbered list items do not match those expected")

      val numberedCircleListItem = doc.getElementsByClass("steps")
      numberedCircleListItem.size shouldBe 1

      val actualCircleListItems = numberedCircleListItem.first().getElementsByTag("li").asScala.toList

      assert(actualCircleListItems.map(_.text) == expectedListItems, "\nActual numbered circle list items do not match those expected")

      val summaryListItem = doc.getElementsByClass("govuk-summary-list")
      summaryListItem.size shouldBe 1

      val summaryListRows = summaryListItem.first().getElementsByClass("govuk-summary-list__row")
      summaryListRows.size shouldBe 2

    }

  }

  "Question Page component" should {

    "generate English html containing an H1, a text only paragraph and a text only bullet point list" in new Test {

      val doc = asDocument(formPageView(questionPage, pageCtx, "question", textFormProvider("url" -> nonEmptyText) )(fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe questionText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Englsh title prefixed by Error: when errors are displayed" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(formPageView(questionPageWithErrors, questionPageContextWithErrs, "question", textFormProvider("url" -> nonEmptyText) )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set radios fieldset aria-describedby correctly whn error occurs" in new Test {

      val questionPageContextWithErrs = questionPageContext.copy(page = questionPageWithErrors)

      val doc = asDocument(formPageView(questionPageWithErrors, questionPageContextWithErrs, "question", textFormProvider("url" -> nonEmptyText) )(fakeRequest, messages))

      val fieldset: Element = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){fset =>
        elementAttrs(fset)("aria-describedby").contains("required-error") shouldBe true
      }
    }

  }

  "Input Page component" should {

    "generate English html containing an H1, a text only paragraph and a text only bullet point list" in new Test {

      val doc = asDocument(formPageView(inputPage, pageCtx, "input",  textFormProvider("12000" -> nonEmptyText)) (fakeRequest, messages))

      checkTitle(doc)

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe inputText.asString

      val paras = doc.select("main.govuk-main-wrapper p")

      paras.size shouldBe 2

      val firstPara = paras.eq(0)
      firstPara.first.text shouldBe openingPara.asString

      val secondPara = paras.eq(1)
      secondPara.first.text shouldBe bulletPointLeadingText.asString

      val actualListItems = doc.select("main.govuk-main-wrapper li").asScala.toList
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.asString, bulletPointTwo.asString, bulletPointThree.asString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate English title prefixed by Error: when errors are displayed" in new Test {

      val inputPageContextWithErrs = inputPageContext.copy(page = inputPageWithErrors)

      val doc = asDocument(formPageView(inputPageWithErrors, inputPageContextWithErrs, "input", textFormProvider("12000" -> nonEmptyText) )(fakeRequest, messages))

      checkTitle(doc, None, Some(messages("error.browser.title.prefix")))
    }

    "set input aria-describedby correctly when error occurs" in new Test {

      val inputPageContextWithErrs = inputPageContext.copy(page = inputPageWithErrors)

      val doc = asDocument(formPageView(inputPageWithErrors, inputPageContextWithErrs, "input", textFormProvider("12000" -> nonEmptyText) )(fakeRequest, messages))

      val inputField: Element = doc.getElementsByTag("input").first
      Option(inputField).fold(fail("Missing fieldset")){inp =>
        elementAttrs(inp)("aria-describedby").contains("required-error") shouldBe true
      }
    }
  }
}
