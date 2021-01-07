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

import forms.{SubmittedDateAnswerFormProvider, SubmittedTextAnswerFormProvider}
import models.ocelot.{Label, LabelCache, Labels, Phrase}
import models.ui.{BulletPointList, CurrencyInput, DateInput, TextInput, FormPage, RequiredErrorMsg, H2, H3, H4, Input, Paragraph, Text}
import org.jsoup._
import org.jsoup.nodes.{Document, Element}
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.data.Forms.nonEmptyText
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html._

import scala.collection.JavaConverters._
import models.PageContext

class InputSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  def elementAttrs(el: Element): Map[String, String] = el.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    def textFormProvider: SubmittedTextAnswerFormProvider = injector.instanceOf[SubmittedTextAnswerFormProvider]
    def dateFormProvider: SubmittedDateAnswerFormProvider = injector.instanceOf[SubmittedDateAnswerFormProvider]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")
    private val para1Text = Text("This is a question", "Welsh, This is a question")
    protected val para1 = Paragraph(para1Text)

    val i1 = Vector("Enter main residence value", "Welsh, Enter main residence value?")
    val i1Hint = Vector("This is where they lived", "Welsh, This is where they lived")

    val inputValue1: String = "200"
    private val leading = Text("You can buy", "Gwallwch brynu")
    private val bp1 = Text("apples", "afalau")
    private val bp2 = Text("oranges", "orennau")
    private val bp3 = Text("pears", "gellyg")
    private val bpList: BulletPointList = BulletPointList(leading, Seq(bp1, bp2, bp3))

    protected val h2: H2 = H2(Text("h2","h2"))
    protected val h3: H3 = H3(Text("h3","h3"))
    protected val h4: H4 = H4(Text("h4","h4"))
    val inputPhrase: Phrase = Phrase(Vector("Some Text", "Welsh, Some Text"))
    val helpPhrase: Phrase = Phrase(Vector("Help text", "Welsh, Help text"))

    val input: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, bpList, para1))

    val inputWithoutBody: Input = CurrencyInput(Text(i1), None, Seq.empty)
    val inputWithHintAndNoBody: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
    protected val errorMsg = RequiredErrorMsg(Text("An error has occurred", "Welsh, An error has occurred"))
    val inputWithErrors: Input = CurrencyInput(Text(i1), None, Seq.empty, Seq(errorMsg))
    val inputWithHintAndErrors: Input = CurrencyInput(Text(i1), Some(Text(i1Hint)), Seq(bpList, para1), Seq(errorMsg))
    implicit val labels: Labels = LabelCache()
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.FormPage("/url", currencyInput)
    val ctx = PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "English Currency Input components" must {

    "render input text as a header" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1(0)
    }

    "render contained paragraphs" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
    }

    "render input with previous answer entered" in new Test {
      private val form = textFormProvider("test" -> nonEmptyText).bind(Map("test" -> inputValue1))
      private val doc = asDocument(components.input(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 1
      private val inputField = elementAttrs(inputs.head)
      inputField("value") shouldBe inputValue1
    }

    "render input with hints" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("span").asScala.toList
      private val firstHint = hints.head
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe Text(i1Hint).value(messages.lang).head.toString
    }

    "Input with no body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "input without body should render hint within a span without a fieldset" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndNoBody, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe None

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint(0)
      }
    }

   "input with hint in error should include hint id and error id in aria-desribedby on input" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndErrors, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("input-hint")
          aria should include("required-error")
        }
      }
   }

    "input with errors should have error class assigned" in new Test {

      private val doc = asDocument(components.input(inputWithErrors, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        val attrs: Map[String, String] = elementAttrs(inp)
        attrs("class").contains("govuk-input") shouldBe true
        attrs("class").contains("govuk-input--width-10") shouldBe true
        attrs("class").contains("govuk-input--error") shouldBe true
      }
    }
    
  }

  "English Text Input component" must {

    trait TextTest extends Test {
      override val input: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, para1))

      override val inputWithoutBody: TextInput = TextInput(Text(i1), None, Seq.empty)
      override val inputWithHintAndNoBody: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
      override val inputWithErrors: TextInput = TextInput(Text(i1), None, Seq.empty, Seq(errorMsg))
      override val inputWithHintAndErrors: TextInput = TextInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(errorMsg))
      val textInput: TextInput = models.ui.TextInput(Text(), None, Seq.empty)
      val textInputPage: FormPage = models.ui.FormPage("/url", textInput)
      override val ctx = PageContext(textInputPage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    }

    "render input text as a header" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1(0)
    }

    "render contained paragraphs" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 1
    }

    "render input with previous answer entered" in new Test {
      private val form = textFormProvider("test" -> nonEmptyText).bind(Map("test" -> inputValue1))
      private val doc = asDocument(components.input(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 1
      private val inputField = elementAttrs(inputs.head)
      inputField("value") shouldBe inputValue1
    }

    "render input with hints" in new Test {
      private val doc = asDocument(components.input(input, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("span").asScala.toList
      private val firstHint = hints.head
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe Text(i1Hint).value(messages.lang).head.toString
    }

    "Input with no body should have a label wrapper class on H1" in new Test {
      private val doc = asDocument(components.input(inputWithoutBody, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-label-wrapper") shouldBe true
    }

    "input without body should render hint within a span without a fieldset" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndNoBody, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe None

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint(0)
      }
    }

   "input with hint in error should include hint id and error id in aria-desribedby on input" in new Test {
      private val doc = asDocument(components.input(inputWithHintAndErrors, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("input-hint")
          aria should include("required-error")
        }
      }
   }

    "input with errors should have error class assigned" in new Test {

      private val doc = asDocument(components.input(inputWithErrors, "test", textFormProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("input").asScala.toList.foreach { inp =>
        val attrs: Map[String, String] = elementAttrs(inp)
        attrs("class").contains("govuk-input") shouldBe true
        attrs("class").contains("govuk-input--width-10") shouldBe true
        attrs("class").contains("govuk-input--error") shouldBe true
      }
    }

  }

  "English Date Input component" must {

    trait DateTest extends Test {
      override val input: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(h2, h3, h4, para1))

      override val inputWithoutBody: DateInput = DateInput(Text(i1), None, Seq.empty)
      override val inputWithHintAndNoBody: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq.empty)
      override val inputWithErrors: DateInput = DateInput(Text(i1), None, Seq.empty, Seq(errorMsg))
      override val inputWithHintAndErrors: DateInput = DateInput(Text(i1), Some(Text(i1Hint)), Seq(para1), Seq(errorMsg))
      val dateInput: DateInput = models.ui.DateInput(Text(), None, Seq.empty)
      val datePage: FormPage = models.ui.FormPage("/url", dateInput)
      override val ctx = PageContext(datePage, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    }

    "render input text as a header" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe i1(0)
    }

    "render contained a fieldset" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset")
      fieldset.size shouldBe 1
    }

    "render contained paragraphs" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render input as input field" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val inputFields = doc.getElementsByTag("input")
      inputFields.size shouldBe 3
    }

    "render input with previous answer entered" in new DateTest {
      private val form = dateFormProvider().bind(Map("day" -> "23", "month" -> "12", "year" -> "2000"))
      private val doc = asDocument(components.input_date(input, "test", form)(fakeRequest, messages, ctx))
      private val inputs = doc.getElementsByTag("input").asScala.toList
      inputs.size shouldBe 3
      private val dayInput = doc.getElementById("day")
      dayInput.`val`() shouldBe "23"
      private val monthInput = doc.getElementById("month")
      monthInput.`val`() shouldBe "12"
      private val yearInput = doc.getElementById("year")
      yearInput.`val`() shouldBe "2000"
    }

    "render input with hints" in new DateTest {
      private val doc = asDocument(components.input_date(input, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val hints = doc.getElementsByTag("span").asScala.toList
      private val firstHint = hints.head
      private val hint1Attrs = elementAttrs(firstHint)
      hint1Attrs("class") shouldBe "govuk-hint"
      firstHint.text() shouldBe Text(i1Hint).value(messages.lang).head.toString
    }

    "render input with no body as a fieldset class on H1" in new DateTest {
      private val doc = asDocument(components.input_date(inputWithoutBody, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val h1 = doc.getElementsByTag("h1").first
      private val attrs = elementAttrs(h1)
      attrs("class").contains("govuk-fieldset__heading") shouldBe true
    }

    "render input without body within a fieldset" in new DateTest {
      private val doc = asDocument(components.input_date(inputWithHintAndNoBody, "test", dateFormProvider())(fakeRequest, messages, ctx))
      private val fieldset = doc.getElementsByTag("fieldset").first

      Option(fieldset) shouldBe Some(fieldset)

      Option(doc.getElementById("input-hint")).fold(fail("Missing hint span")) { span =>
        val attrs = elementAttrs(span)
        attrs("id") shouldBe "input-hint"
        attrs("class").contains("govuk-hint") shouldBe true
        span.text shouldBe i1Hint(0)
      }
    }

    "render inputs with error class for erroneous input" in new DateTest {

      private val doc: Document = asDocument(components.input_date(inputWithErrors, "test", dateFormProvider())(fakeRequest, messages, ctx))

      private val commonInputClasses: List[String] = List( "govuk-input", "govuk-date-input__input", "govuk-input--error")

      private val dayInput: Element = doc.getElementById("day")

      private val dayAttrs: Map[String, String] = elementAttrs(dayInput)

      commonInputClasses.foreach{ cic => dayAttrs("class").contains(cic) shouldBe true}

      dayAttrs("class").contains("govuk-input--width-2") shouldBe true

      private val monthInput: Element = doc.getElementById("month")

      private val monthAttrs: Map[String, String] = elementAttrs(monthInput)

      commonInputClasses.foreach{ cic => monthAttrs("class").contains(cic) shouldBe true}

      monthAttrs("class").contains("govuk-input--width-2") shouldBe true

      private val yearInput: Element = doc.getElementById("year")

      private val yearAttrs: Map[String, String] = elementAttrs(yearInput)

      commonInputClasses.foreach{ cic => yearAttrs("class").contains(cic) shouldBe true}

      yearAttrs("class").contains("govuk-input--width-4") shouldBe true
    }
  }
}
