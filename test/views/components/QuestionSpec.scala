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
import play.api.data.Forms.nonEmptyText
import play.api.inject.Injector
import play.api.i18n.{Messages, MessagesApi, Lang}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup._
import views.html._
import forms.SubmittedTextAnswerFormProvider
import models.ui.{Paragraph, Text, Question, Answer, BulletPointList, ErrorMsg}
import models.ocelot.{Labels, LabelCache}
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._

class QuestionSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  def elementAttrs(el: Element): Map[String, String] = el.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    def formProvider: SubmittedTextAnswerFormProvider = injector.instanceOf[SubmittedTextAnswerFormProvider]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")
    val para1Text = Text("This is a question", "Welsh, This is a question")
    val para1 = Paragraph(para1Text)

    val q1 = Vector("Do you agree?", "Welsh, Do you agree?")
    val ans1 = Vector("Yes", "Welsh, Yes")
    val ans2 = Vector("No", "Welsh, Yes")
    val ans3 = Vector("Not sure", "Welsh, Yes")

    val ans1Hint = Vector("You agree with the assertion", "Welsh, You agree with the assertion")
    val ans2Hint = Vector("You DONT agree with the assertion", "Welsh, You DONT agree with the assertion")
    val ans3Hint = Vector("You dont know", "Welsh, You dont know")
    val ansIndexZero = "0"
    val a1 = Answer(Text(ans1), Some(Text(ans1Hint)))
    val a2 = Answer(Text(ans2), Some(Text(ans2Hint)))
    val a3 = Answer(Text(ans3), Some(Text(ans3Hint)))
    val leading = Text("You can buy", "Gwallwch brynu")
    val bp1 = Text("apples", "afalau")
    val bp2 = Text("oranges", "orennau")
    val bp3 = Text("pears", "gellyg")
    val bpList: BulletPointList = BulletPointList(leading, Seq(bp1, bp2, bp3))
    val questionHint = Vector("Is it Yes or is it No?", "Welsh, Is it Yes or is it No?")

    val answers = Seq(a1, a2, a3)
    val horizontalAnswers = Seq(a1.copy(hint = None), a2.copy(hint = None))
    val question = Question(Text(q1), None, Seq(bpList, para1), answers)
    val questionWithHorizontalAnswers = Question(Text(q1), None, Seq(para1), horizontalAnswers)
    val questionWithoutBody = Question(Text(q1), None, Seq.empty, answers)
    val questionWithHint = Question(Text(q1), Some(Text(questionHint)), Seq(bpList, para1), answers)
    val questionWithHintAndNoBody = Question(Text(q1), Some(Text(questionHint)), Seq.empty, answers)
    val errorMsg = ErrorMsg("id", Text("An error has occurred", "Welsh, An error has occurred"))
    val questionWithHintAndErrors = Question(Text(q1), Some(Text(questionHint)), Seq(bpList, para1), answers, Seq(errorMsg))
    implicit val labels: Labels = LabelCache()
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.InputPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "English Question component" must {

    "render question text as a header" in new Test {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe q1(0)
    }

    "render contained paragraphs" in new Test {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render answers as radio buttons" in new Test {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val radios = doc.getElementsByTag("input")
      radios.size shouldBe answers.length
      val radioLabels = doc.getElementsByTag("label").asScala.map(_.text()).toList
      radioLabels.size shouldBe answers.length
      radioLabels(0) shouldBe Text(ans1).value(messages.lang).head.toString
      radioLabels(1) shouldBe Text(ans2).value(messages.lang).head.toString
      radioLabels(2) shouldBe Text(ans3).value(messages.lang).head.toString
    }

    "render answers as radio buttons with previous answer selected" in new Test {
      val form = formProvider("test" -> nonEmptyText).bind(Map("test" -> ansIndexZero))
      val doc = asDocument(components.question(question, "test", form)(fakeRequest, messages, ctx))
      val radios = doc.getElementsByTag("input").asScala.toList
      radios.size shouldBe answers.length
      elementAttrs(radios.head).contains("checked") shouldBe true
    }

    "render answers with hints vertically" in new Test {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val hints = doc.getElementsByTag("span").asScala.toList

      val hint1Attrs = elementAttrs(hints(0))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(0).text() shouldBe Text(ans1Hint).value(messages.lang).head.toString
      val hint2Attrs = elementAttrs(hints(1))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(1).text() shouldBe Text(ans2Hint).value(messages.lang).head.toString
      val hint3Attrs = elementAttrs(hints(2))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(2).text() shouldBe Text(ans3Hint).value(messages.lang).head.toString
    }

    "render answers with hints horizontally" in new Test {
      val doc = asDocument(components.question(questionWithHorizontalAnswers, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      hints shouldBe Nil
      val divs = doc.getElementsByTag("div").asScala.toList.filter { div =>
        val attrs = elementAttrs(div)
        attrs("class").contains("govuk-radios")
      }
      divs.headOption.map { div =>
        val attrs = elementAttrs(div)
        attrs("class").contains("govuk-radios--inline") shouldBe true
      } orElse {
        fail("Missing govuk-radios--inline, answers not horizontal")
      }
    }

    "question with no body should hide the legend heading" in new Test {
      val doc = asDocument(components.question(questionWithoutBody, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val legend = doc.getElementsByTag("legend").first
      val attrs = elementAttrs(legend)

      attrs("class").contains("govuk-fieldset__legend") shouldBe true
      attrs("class").contains("govuk-visually-hidden") shouldBe false
    }

    "question with body should render hint within a span within fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
        Option(fset.getElementsByTag("span").first).fold(fail("Missing hint span within fieldset")) { span =>
          val attrs = elementAttrs(span)
          attrs("id") shouldBe "question-hint"
          attrs("class").contains("govuk-hint") shouldBe true
          span.text shouldBe questionHint(0)
        }
      }
    }

    "question without body should render hint within a span within fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHintAndNoBody, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
        Option(fset.getElementsByTag("span").first).fold(fail("Missing hint span within fieldset")) { span =>
          val attrs = elementAttrs(span)
          attrs("id") shouldBe "question-hint"
          attrs("class").contains("govuk-hint") shouldBe true
          span.text shouldBe questionHint(0)
        }
      }
    }

   "question with hint should include hint id in aria-desribedby on fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
      }
   }

   "question with hint in error should include hint id and error id in aria-desribedby on fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHintAndErrors, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("question-hint")
          aria should include("id-error")
        }
      }
   }

   "answer with hint should include hint id in aria-desribedby on input" in new Test {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      for{
        (inp, index) <- doc.getElementsByTag("input").asScala.toList.zipWithIndex
      } yield {
        elementAttrs(inp)("aria-describedby") shouldBe s"test-item-$index-hint"
      }
   }

   "answer with hint in error should include hint id and error id in aria-desribedby on input" in new Test {
      val doc = asDocument(components.question(questionWithHintAndErrors, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      for{
        (inp, index) <- doc.getElementsByTag("input").asScala.toList.zipWithIndex
      } yield {
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include(s"test-item-$index-hint")
          aria should include(s"id-error")
        }
      }
   }

  }

  "Welsh Question component" must {

    "render question text as a header" in new WelshTest {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe q1(1)
    }

    "render contained paragraphs" in new WelshTest {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))

      doc.getElementsByTag("p").asScala.toList.foreach { p =>
        elementAttrs(p)("class").contains("govuk-body") shouldBe true
      }
    }

    "render answers as radio buttons" in new WelshTest {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val radios = doc.getElementsByTag("input")
      radios.size shouldBe answers.length
      val radioLabels = doc.getElementsByTag("label").asScala.map(_.text()).toList
      radioLabels.size shouldBe answers.length
      radioLabels(0) shouldBe Text(ans1).value(messages.lang).head.toString
      radioLabels(1) shouldBe Text(ans2).value(messages.lang).head.toString
      radioLabels(2) shouldBe Text(ans3).value(messages.lang).head.toString
    }

    "render answers as radio buttons with previous answer selected" in new Test {
      val form = formProvider("test" -> nonEmptyText).bind(Map("test" -> ansIndexZero))
      val doc = asDocument(components.question(question, "test", form)(fakeRequest, messages, ctx))
      val radios = doc.getElementsByTag("input").asScala.toList
      radios.size shouldBe answers.length
      elementAttrs(radios.head).contains("checked") shouldBe true
    }

    "render answers with hints" in new WelshTest {
      val doc = asDocument(components.question(question, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val hints = doc.getElementsByTag("span").asScala.toList

      val hint1Attrs = elementAttrs(hints(0))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(0).text() shouldBe Text(ans1Hint).value(messages.lang).head.toString
      val hint2Attrs = elementAttrs(hints(1))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(1).text() shouldBe Text(ans2Hint).value(messages.lang).head.toString
      val hint3Attrs = elementAttrs(hints(2))
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(2).text() shouldBe Text(ans3Hint).value(messages.lang).head.toString
    }

    "render answers with hints horizontally" in new WelshTest {
      val doc = asDocument(components.question(questionWithHorizontalAnswers, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      hints shouldBe Nil
      val divs = doc.getElementsByTag("div").asScala.toList.filter { div =>
        val attrs = elementAttrs(div)
        attrs("class").contains("govuk-radios")
      }
      divs.headOption.map { div =>
        val attrs = elementAttrs(div)
        attrs("class").contains("govuk-radios--inline") shouldBe true
      } orElse {
        fail("Missing govuk-radios--inline, answers not horizontal")
      }
    }

    "question with no body should hide the legend heading" in new WelshTest {
      val doc = asDocument(components.question(questionWithoutBody, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val legend = doc.getElementsByTag("legend").first
      val attrs = elementAttrs(legend)

      attrs("class").contains("govuk-fieldset__legend") shouldBe true
      attrs("class").contains("govuk-visually-hidden") shouldBe false
    }

    "question with body should render hint within a span within fieldset" in new WelshTest {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
        Option(fset.getElementsByTag("span").first).fold(fail("Missing hint span within fieldset")) { span =>
          val attrs = elementAttrs(span)
          attrs("id") shouldBe "question-hint"
          attrs("class").contains("govuk-hint") shouldBe true
          span.text shouldBe questionHint(1)
        }
      }
    }

    "question without body should render hint within a span within fieldset" in new WelshTest {
      val doc = asDocument(components.question(questionWithHintAndNoBody, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
        Option(fset.getElementsByTag("span").first).fold(fail("Missing hint span within fieldset")) { span =>
          val attrs = elementAttrs(span)
          attrs("id") shouldBe "question-hint"
          attrs("class").contains("govuk-hint") shouldBe true
          span.text shouldBe questionHint(1)
        }
      }
    }

   "question with hint should include hint id in aria-desribedby on fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset)("aria-describedby") shouldBe "question-hint"
      }
   }

   "question with hint in error should include hint id and error id in aria-desribedby on fieldset" in new Test {
      val doc = asDocument(components.question(questionWithHintAndErrors, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      val fieldset = doc.getElementsByTag("fieldset").first
      Option(fieldset).fold(fail("Missing fieldset")){ fset =>
        elementAttrs(fset).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include("question-hint")
          aria should include("id-error")
        }
      }
   }

   "answer with hint should include hint id in aria-desribedby on input" in new Test {
      val doc = asDocument(components.question(questionWithHint, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      for{
        (inp, index) <- doc.getElementsByTag("input").asScala.toList.zipWithIndex
      } yield {
        elementAttrs(inp)("aria-describedby") shouldBe s"test-item-$index-hint"
      }
   }

   "answer with hint in error should include hint id and error id in aria-desribedby on input" in new Test {
      val doc = asDocument(components.question(questionWithHintAndErrors, "test", formProvider("test" -> nonEmptyText))(fakeRequest, messages, ctx))
      for{
        (inp, index) <- doc.getElementsByTag("input").asScala.toList.zipWithIndex
      } yield {
        elementAttrs(inp).get("aria-describedby").fold(fail("Missing aria-describedby")){ aria =>
          aria should include(s"test-item-$index-hint")
          aria should include(s"id-error")
        }
      }
   }

  }
}
