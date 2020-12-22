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

import models.PageContext
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html.components.{h1_heading, h2_heading, h3_heading, paragraph}
import models.ocelot.{Label, LabelCache, Labels}
import models.ui.{Currency, CurrencyInput, CurrencyPoundsOnly, DateStandard, FormPage, H1, H2, H3, LabelRef, Link, Paragraph, Text, Txt, Words}
import org.jsoup.nodes.{Document, Element}
import play.api.mvc.AnyContentAsEmpty

import scala.collection.JavaConverters._

class RenderTextSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    implicit val labels: Labels = LabelCache(Map("Blah" -> Label("Blah", Some("a value")),
                                                 "A-Label" -> Label("A-Label", Some("33.9")),
                                                 "Date-Label" -> Label("Date-Label", Some("29/2/2020")),
                                                 "BigNumber" -> Label("BigNumber", Some("12345678")),
                                                 "BigNumberDps" -> Label("BigNumber", Some("12345678.45"))))
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/")

    val boldText: Text = Text(Words("Hello", bold = true), Words("Welsh Hello", bold = true))
    val normalText: Text = Text(Words("Hello"), Words("Welsh Hello"))

    val textWithLabelRef: Text = Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(" , price "), LabelRef("A-Label")),
                                      Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(" , price "),LabelRef("A-Label")))
    val textWithBoldLabelRef: Text = Text(Seq(Words("A label must have "),LabelRef("Blah", Txt, bold = true), Words(" , price "), LabelRef("A-Label")),
                                          Seq(Words("Welsh A label must have "),LabelRef("Blah", Txt, bold = true), Words(" , price "),LabelRef("A-Label")))
    val textWithNonExistentLabelRef: Text = Text(Seq(Words("The price is "),LabelRef("BLAHBLAH", Currency)),
                                                 Seq(Words("Welsh The price is "),LabelRef("BLAHBLAH", Currency)))
    val textWithNonExistentCurrencyPoundsOnlyLabelRef: Text = Text(Seq(Words("The price is "),LabelRef("BLAHBLAH", CurrencyPoundsOnly)),
                                                                   Seq(Words("Welsh The price is "),LabelRef("BLAHBLAH", CurrencyPoundsOnly)))
    val textWithCurrencyLabelRef: Text = Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(" , price "), LabelRef("A-Label", Currency)),
                                              Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(" , price "),LabelRef("A-Label", Currency)))
    val textWithCurrencyPOLabelRef: Text = Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(" , price "), LabelRef("A-Label", CurrencyPoundsOnly)),
                                                Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(" , price "),LabelRef("A-Label", CurrencyPoundsOnly)))
    val textWithBoldCurrencyPOLabelRef: Text =
      Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(" , price "), LabelRef("A-Label", CurrencyPoundsOnly, bold = true)),
           Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(" , price "),LabelRef("A-Label", CurrencyPoundsOnly, bold = true)))
    val textWithInvalidCurrencyLabelRef: Text =
      Text(Seq(Words("A label must have "),LabelRef("Blah", Currency), Words(" , price "), LabelRef("A-Label", Currency)),
           Seq(Words("Welsh A label must have "),LabelRef("Blah", Currency), Words(" , price "), LabelRef("A-Label", Currency)))
    val textWithInvalidCurrencyPoundsOnlyLabelRef: Text =
      Text(Seq(Words("A label must have "),LabelRef("Blah", CurrencyPoundsOnly), Words(" , price "), LabelRef("A-Label", CurrencyPoundsOnly)),
           Seq(Words("Welsh A label must have "),LabelRef("Blah", CurrencyPoundsOnly), Words(" , price "), LabelRef("A-Label", CurrencyPoundsOnly)))
    val textLargeValueNoDpsCurrencyLabelRef: Text =
      Text(Seq(Words("A large number stored without decimal places, but rendered with .00, "), LabelRef("BigNumber", Currency)),
           Seq(Words("Welsh A large number stored without decimal places, but rendered with .00, "), LabelRef("BigNumber", Currency)))
    val textLargeValueDpsCurrencyPOLabelRef: Text =
      Text(Seq(Words("A large number stored without decimal places, but rendered with .00, "), LabelRef("BigNumberDps", CurrencyPoundsOnly)),
           Seq(Words("Welsh A large number stored without decimal places, but rendered with .00, "), LabelRef("BigNumberDps", CurrencyPoundsOnly)))
    val textWithStandardDateLabelRef: Text =
      Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(", date "), LabelRef("Date-Label", DateStandard, bold = true)),
           Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(", date "),LabelRef("Date-Label", DateStandard, bold = true)))
    val textWithStandardDateLabelRefWithInvalidDate: Text =
      Text(Seq(Words("A label must have "),LabelRef("Blah"), Words(", date "), LabelRef("A-Label", DateStandard, bold = true)),
           Seq(Words("Welsh A label must have "),LabelRef("Blah"), Words(", date "),LabelRef("A-Label", DateStandard, bold = true)))

    val currencyInput: CurrencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page: FormPage = models.ui.FormPage("/url", currencyInput)
    implicit val ctx: PageContext = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "render_text component" should {

    "generate English html containing label references with default output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have a value , price 33.9"
    }

    "generate English html containing bold label references with default output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithBoldLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      val boldElement = p.getElementsByTag("strong").first
      boldElement.text shouldBe "a value"
    }

    "generate English html containing bold label references with CurrencyPoundsOnly output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithBoldCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      val boldElement = p.getElementsByTag("strong").first
      boldElement.text shouldBe "£33"
    }

    "generate English html containing label reference to Label which does not exist" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithNonExistentLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "The price is"
    }

    "generate English html containing label reference to CurrencyPoundsOnly Label which does not exist" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithNonExistentCurrencyPoundsOnlyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "The price is"
    }

    "generate English html containing label references with Currency output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have a value , price £33.90"
    }

    "generate English html containing label references with CurrencyPoundsOnly output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have a value , price £33"
    }

    "generate English html containing label references with Currency output formatting with invalid data" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithInvalidCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have , price £33.90"
    }

    "generate English html containing label references with CurrencyPoundsOnly output formatting with invalid data" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithInvalidCurrencyPoundsOnlyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have , price £33"
    }

    "generate English html containing label references with Currency output formatting with large values and no decimal places" in new Test {
      val doc = asDocument(paragraph(Paragraph(textLargeValueNoDpsCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A large number stored without decimal places, but rendered with .00, £12,345,678.00"
    }

    "generate English html containing label references with CurrencyPoundsOnly output formatting with large values and decimal places" in new Test {
      val doc = asDocument(paragraph(Paragraph(textLargeValueDpsCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A large number stored without decimal places, but rendered with .00, £12,345,678"
    }

    "generate English html containing normal text" in new Test {
      val doc = asDocument(paragraph(Paragraph(normalText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 0
    }

    "generate English html containing an H1" in new Test {
      val doc = asDocument(h1_heading(H1(normalText)))
      val h1 = doc.getElementsByTag("h1")
      h1.size shouldBe 1
    }

    "generate English html containing an H2" in new Test {
      val doc = asDocument(h2_heading(H2(normalText)))
      val h2 = doc.getElementsByTag("h2")
      h2.size shouldBe 1
    }

    "generate English html containing an H3" in new Test {
      val doc = asDocument(h3_heading(H3(normalText)))
      val h3 = doc.getElementsByTag("h3")
      h3.size shouldBe 1
    }

    "generate English html containing a normal text paragraph" in new Test {
      val doc = asDocument(paragraph(Paragraph(boldText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 1
    }

    "generate Welsh html containing label references with default output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have a value , price 33.9"
    }

    "generate Welsh html containing bold label references with default output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithBoldLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      val boldElement = p.getElementsByTag("strong").first
      boldElement.text shouldBe "a value"
    }

    "generate Welsh html containing bold label references with CurrencyPoundsOnly output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithBoldCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      val boldElement = p.getElementsByTag("strong").first
      boldElement.text shouldBe "£33"
    }

    "generate Welsh html containing label reference to Label which does not exist" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithNonExistentLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh The price is"
    }

    "generate Welsh html containing label reference to CurrencyPoundsOnly Label which does not exist" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithNonExistentCurrencyPoundsOnlyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh The price is"
    }

    "generate Welsh html containing label references with Currency output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have a value , price £33.90"
    }

    "generate Welsh html containing label references with CurrencyPoundsOnly output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have a value , price £33"
    }

    "generate Welsh html containing label references with Currency output formatting with invalid data" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithInvalidCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have , price £33.90"
    }

    "generate Welsh html containing label references with CurrencyPoundsOnly output formatting with invalid data" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithInvalidCurrencyPoundsOnlyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have , price £33"
    }

    "generate Welsh html containing label references with Currency output formatting with large values and no decimal places" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textLargeValueNoDpsCurrencyLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A large number stored without decimal places, but rendered with .00, £12,345,678.00"
    }

    "generate Welsh html containing label references with CurrencyPoundsOnly output formatting with large values and decimal places" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textLargeValueDpsCurrencyPOLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A large number stored without decimal places, but rendered with .00, £12,345,678"
    }

    "generate Welsh html containing normal text" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(normalText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 0
    }

    "generate Welsh html containing an H1" in new WelshTest {
      val doc = asDocument(h1_heading(H1(normalText)))
      val h1 = doc.getElementsByTag("h1")
      h1.size shouldBe 1
    }

    "generate Welsh html containing an H2" in new WelshTest {
      val doc = asDocument(h2_heading(H2(normalText)))
      val h2 = doc.getElementsByTag("h2")
      h2.size shouldBe 1
    }

    "generate Welsh html containing an H3" in new WelshTest {
      val doc = asDocument(h3_heading(H3(normalText)))
      val h3 = doc.getElementsByTag("h3")
      h3.size shouldBe 1
    }

    "generate Welsh html containing a normal text paragraph" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(boldText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 1
    }

    "test links with text to ensure correct spacing" in new Test {
      val link1 = Link("this/is/a/link", "Link Text")
      val words1 = Words("this is the first section ")
      val words2 = Words(", second section should follow link text.")
      val phrases1 = Seq(words1, link1, words2)
      val text = Text(phrases1, phrases1)
      val doc = asDocument(paragraph(Paragraph(text)))
      val pTag = doc.getElementsByTag("p")

      pTag.text() shouldBe "this is the first section Link Text, second section should follow link text."
    }

    "generate English html containing label reference with Standard Date output formatting" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithStandardDateLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have a value, date 29 February 2020"
    }
    "generate Welsh html containing label reference with Standard Date output formatting" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithStandardDateLabelRef))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have a value, date 29 February 2020"
    }
    "generate English html containing label reference with Standard Date output formatting with invalid date" in new Test {
      val doc = asDocument(paragraph(Paragraph(textWithStandardDateLabelRefWithInvalidDate))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "A label must have a value, date 33.9"
    }
    "generate Welsh html containing label reference with Standard Date output formatting with invalid date" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(textWithStandardDateLabelRefWithInvalidDate))(messages, ctx))
      val p = doc.getElementsByTag("p").first
      p.text shouldBe "Welsh A label must have a value, date 33.9"
    }


  }
}
