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
import views.html.components.paragraph
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
    val para1 = Paragraph(Seq(para1Text))

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
    val question = Question(Text(q1), Seq(para1), answers, false)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }
}
