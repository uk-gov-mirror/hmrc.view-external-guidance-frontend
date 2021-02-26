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
import views.html._
import forms.SubmittedTextAnswerFormProvider
import models.ui.{FormPage, H2, Paragraph, Sequence, Text}
import core.models.ocelot.{LabelCache, Labels}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import base.{ViewFns, ViewSpec}
import models.PageContext

class SequenceSpec extends WordSpec with Matchers with ViewSpec with ViewFns with GuiceOneAppPerSuite {

  trait Test {
    private def injector: Injector = app.injector

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    def formProvider: SubmittedTextAnswerFormProvider = injector.instanceOf[SubmittedTextAnswerFormProvider] // TODO This may change

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    val fakeRequest = FakeRequest("GET", "/")

    implicit val labels: Labels = LabelCache()

    // Create test sequence data
    val sequenceTitle: Text = Text("Select your favourite sweets")
    val sequenceHint: Text = Text("Consider both chocolate and sugar style sweets")
    val sweetOptions: Seq[Text] = Seq(Text("Wine gums"), Text("Munchies"))

    val h2: H2 = H2(Text("Subtitle"))
    val p: Paragraph = Paragraph(Text("Introduction to sweets"))

    val sequenceWithoutHint: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptions,
      Seq.empty,
      Seq.empty
    )

    val sequenceWithHint: Sequence = Sequence(
      sequenceTitle,
      Some(sequenceHint),
      sweetOptions,
      Seq.empty,
      Seq.empty
    )

    val sequenceWithBody: Sequence = Sequence(
      sequenceTitle,
      None,
      sweetOptions,
      Seq(h2, p),
      Seq.empty
    )

    val pageWithoutHint: FormPage = FormPage("/start", sequenceWithoutHint)
    val pageWithHint: FormPage = FormPage("/start", sequenceWithHint)
    val pageWithBody: FormPage = FormPage("/start", sequenceWithBody)

    val pageWithoutHintCtx: PageContext = PageContext(
      pageWithoutHint,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithHintCtx: PageContext = PageContext(
      pageWithHint,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    val pageWithBodyCtx: PageContext = PageContext(
      pageWithBody,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )
  }

  "Sequence component" must {

    "render sequence title for sequence without body inside legend" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHint, "test", formProvider("test" -> nonEmptyText))
        (fakeRequest, messages, pageWithoutHintCtx)
      )

      val legends: Elements = doc.getElementsByTag("legend")

      legends.size shouldBe 1

      val headings: Elements = legends.first.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text() shouldBe sequenceTitle.asString

      elementAttrs(headings.first)("class") shouldBe "govuk-fieldset__heading"
    }

    "render sequence hint for sequence when present" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithHint, "test", formProvider("test" -> nonEmptyText))
        (fakeRequest, messages, pageWithHintCtx)
      )

      val hint: Option[Element] = getElementById(doc, "sequence-hint")

      hint match {
        case Some(elem) =>
          elem.text() shouldBe sequenceHint.asString
          elementAttrs(elem)("class") shouldBe "govuk-hint"
        case None => fail("Sequence should contain hint text")
      }

    }

    "sequence with hint should include hint id in aria-describedBy on field set" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithHint, "test", formProvider("test" -> nonEmptyText))
        (fakeRequest, messages, pageWithHintCtx)
      )

      val fieldSets: Elements = doc.getElementsByTag("fieldset")

      fieldSets.size shouldBe 1

      elementAttrs(fieldSets.first)("aria-describedby") shouldBe "sequence-hint"
    }

    "render title for sequence with body outside legend" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithBody, "test", formProvider("test" -> nonEmptyText))
        (fakeRequest, messages, pageWithBodyCtx)
      )

      val legends: Elements = doc.getElementsByTag("legend")

      legends.size shouldBe 1

      val headingsInsideLegend: Elements = legends.first.getElementsByTag("h1")

      headingsInsideLegend.size shouldBe 0

      val headings: Elements = doc.getElementsByTag("h1")

      headings.size shouldBe 1

      headings.first.text shouldBe sequenceTitle.asString

      elementAttrs(headings.first)("class") shouldBe "govuk-heading-xl"

      val h2Elements: Elements = doc.getElementsByTag("h2")

      h2Elements.size shouldBe 1

      h2Elements.first.text() shouldBe h2.text.asString
      elementAttrs(h2Elements.first)("class") shouldBe "govuk-heading-l"

      val paragraphElements: Elements = doc.getElementsByTag("p")

      paragraphElements.size shouldBe 1

      paragraphElements.first.text() shouldBe p.text.asString
      elementAttrs(paragraphElements.first)("class") shouldBe "govuk-body"
    }

    "render a checkbox for each sequence option" in new Test {

      val doc: Document = asDocument(
        components.sequence(sequenceWithoutHint, "test", formProvider("test" -> nonEmptyText))
        (fakeRequest, messages, pageWithoutHintCtx)
      )

      val checkboxContainerDiv: Element = getSingleElementByClass(doc, "govuk-checkboxes")

      val checkboxDivs: Elements = checkboxContainerDiv.getElementsByClass("govuk-checkboxes__item")

      checkboxDivs.size shouldBe 2

      // Check constant attributes on first checkbox
      val checkbox1Inputs: Elements = checkboxDivs.first.getElementsByTag("input")

      checkbox1Inputs.size shouldBe 1

      val checkbox1InputAttrs: Map[String, String] = elementAttrs(checkbox1Inputs.first)

      val checkbox1Labels: Elements = checkboxDivs.first.getElementsByTag("label")

      checkbox1Labels.size shouldBe 1

      val checkbox1LabelAttrs: Map[String, String] = elementAttrs(checkbox1Labels.first)

      checkbox1InputAttrs("class") shouldBe "govuk-checkboxes__input"
      checkbox1InputAttrs("name") shouldBe "test"

      checkbox1LabelAttrs("class") shouldBe "govuk-label govuk-checkboxes__label"

      // Check variable attributes on all checkboxes
      val checkbox2Inputs: Elements = checkboxDivs.last.getElementsByTag("input")

      checkbox2Inputs.size shouldBe 1

      val checkbox2InputAttrs: Map[String, String] = elementAttrs(checkbox2Inputs.first)

      val checkbox2Labels: Elements = checkboxDivs.last.getElementsByTag("label")

      checkbox2Labels.size shouldBe 1

      val checkbox2LabelAttrs: Map[String, String] = elementAttrs(checkbox2Labels.first)

      checkbox1InputAttrs("id") shouldBe "test[0]"
      checkbox1InputAttrs("value") shouldBe "0"
      checkbox1LabelAttrs("for") shouldBe "test[0]"
      checkbox1Labels.first.text() shouldBe sweetOptions.head.asString

      checkbox2InputAttrs("id") shouldBe "test[1]"
      checkbox2InputAttrs("value") shouldBe "1"
      checkbox2LabelAttrs("for") shouldBe "test[1]"
      checkbox2Labels.first.text shouldBe sweetOptions.last.asString
    }

  }

}
