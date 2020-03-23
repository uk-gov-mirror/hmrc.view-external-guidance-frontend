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
import play.api.i18n.{Messages, MessagesApi, Lang}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html._
import models.ui.{Paragraph,Text, Question, Answer}
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._


class QuestionSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
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
    val a1 = Answer(Text(ans1), Some(Text(ans1Hint)), "/yes")
    val a2 = Answer(Text(ans2), Some(Text(ans2Hint)), "/no")
    val a3 = Answer(Text(ans3), Some(Text(ans3Hint)), "/dontknow")
    val answers = Seq(a1, a2, a3)
    val horizontalAnswers = Seq(a1.copy(hint = None),a2.copy(hint = None))
    val question = Question(Text(q1), Seq(para1), answers)

    val questionWithHorizontalAnswers = Question(Text(q1), Seq(para1), horizontalAnswers)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }


  "English Question component" must {

    "render question text as a header" in new Test {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe q1(0)
    }

    "render contained paragraph as hints" in new Test {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val hint = doc.getElementsByTag("span").first
      val hintAttrs = hint.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hintAttrs("class") shouldBe "govuk-hint"
      hint.text() shouldBe para1Text.value(messages.lang).head.toString
    }

    "render answers as radio buttons" in new Test {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val radios = doc.getElementsByTag("input")
      radios.size shouldBe answers.length
      val radioLabels = doc.getElementsByTag("label").asScala.map(_.text()).toList
      radioLabels.size shouldBe answers.length
      radioLabels(0) shouldBe Text(ans1).value(messages.lang).head.toString
      radioLabels(1) shouldBe Text(ans2).value(messages.lang).head.toString
      radioLabels(2) shouldBe Text(ans3).value(messages.lang).head.toString
    }

    "render answers with hints vertically" in new Test {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      val hint1Attrs = hints(0).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(0).text() shouldBe Text(ans1Hint).value(messages.lang).head.toString
      val hint2Attrs = hints(1).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(1).text() shouldBe Text(ans2Hint).value(messages.lang).head.toString
      val hint3Attrs = hints(2).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(2).text() shouldBe Text(ans3Hint).value(messages.lang).head.toString
    }

    "render answers with hints horizontally" in new Test {
      val doc = asDocument(components.question(questionWithHorizontalAnswers, "test")(fakeRequest, messages))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      hints shouldBe Nil
      val divs = doc.getElementsByTag("div").asScala.toList.filter{ div =>
        val attrs = div.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
        attrs("class").contains("govuk-radios")
      }
      divs.headOption.map{ div =>
        val attrs = div.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
        attrs("class").contains("govuk-radios--inline") shouldBe true
      } orElse {
        fail("Missing govuk-radios--inline, answers not horizontal")
      }
    }

  }

  "Welsh Question component" must {

    "render question text as a header" in new WelshTest {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val heading = doc.getElementsByTag("h1")
      heading.size shouldBe 1
      heading.first.text() shouldBe q1(1)
    }

    "render contained paragraph as hints" in new WelshTest {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val hint = doc.getElementsByTag("span").first
      val hintAttrs = hint.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hintAttrs("class") shouldBe "govuk-hint"
      hint.text() shouldBe para1Text.value(messages.lang).head.toString
    }

    "render answers as radio buttons" in new WelshTest {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val radios = doc.getElementsByTag("input")
      radios.size shouldBe answers.length
      val radioLabels = doc.getElementsByTag("label").asScala.map(_.text()).toList
      radioLabels.size shouldBe answers.length
      radioLabels(0) shouldBe Text(ans1).value(messages.lang).head.toString
      radioLabels(1) shouldBe Text(ans2).value(messages.lang).head.toString
      radioLabels(2) shouldBe Text(ans3).value(messages.lang).head.toString
    }

    "render answers with hints" in new WelshTest {
      val doc = asDocument(components.question(question, "test")(fakeRequest, messages))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      val hint1Attrs = hints(0).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(0).text() shouldBe Text(ans1Hint).value(messages.lang).head.toString
      val hint2Attrs = hints(1).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(1).text() shouldBe Text(ans2Hint).value(messages.lang).head.toString
      val hint3Attrs = hints(2).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      hint1Attrs("class") shouldBe "govuk-hint govuk-radios__hint"
      hints(2).text() shouldBe Text(ans3Hint).value(messages.lang).head.toString
    }

    "render answers with hints horizontally" in new Test {
      val doc = asDocument(components.question(questionWithHorizontalAnswers, "test")(fakeRequest, messages))
      val hints = doc.getElementsByTag("span").asScala.toList.drop(1)

      hints shouldBe Nil
      val divs = doc.getElementsByTag("div").asScala.toList.filter{ div =>
        val attrs = div.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
        attrs("class").contains("govuk-radios")
      }
      divs.headOption.map{ div =>
        val attrs = div.attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
        attrs("class").contains("govuk-radios--inline") shouldBe true
      } orElse {
        fail("Missing govuk-radios--inline, answers not horizontal")
      }
    }

  }
}