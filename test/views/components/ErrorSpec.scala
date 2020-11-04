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
import models.ui.{Paragraph, Text, Question, Answer, ErrorMsg}
import models.ocelot.LabelCache
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._

class ErrorSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val headStrings = Vector("HEADING", "Welsh, HEADING")
    val errorStrings = Vector("Please select", "Welsh, Please")
    val fakeRequest = FakeRequest("GET", "/")
    val heading = Text(headStrings)
    val errorMsgs = Seq(ErrorMsg("id", Text(errorStrings)))
    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.InputPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode")
  }

  "error_summary" must {

    "render error header and list of messages" in new Test {
      val doc = asDocument(components.error_summary(heading, "inputName", errorMsgs)(messages, ctx))
      val head = doc.getElementById("error-summary-title")

      head.text() shouldBe messages("error.summary.title")

      val lists = doc.getElementsByTag("ul").asScala.toList
      val listItems = lists(0).getElementsByTag("li").asScala.toList

      listItems(0).text() shouldBe errorStrings(0)
    }

  }

  "error_message" must {

    "render error message" in new Test {
      val doc = asDocument(components.error_message(errorMsgs)(messages, ctx))
      val span = doc.getElementsByTag("span").asScala.toList.filter(_.id == "id-error")

      span(0).text() shouldBe messages("error.browser.title.prefix") + " " + errorStrings(0)
    }

    "render hidden text with error message" in new Test {

      val doc = asDocument(components.error_message(errorMsgs)(messages, ctx))
      val hidden = doc.getElementsByClass("govuk-visually-hidden").asScala.toList

      hidden(0).text() shouldBe messages("error.browser.title.prefix")
    }

  }

}
