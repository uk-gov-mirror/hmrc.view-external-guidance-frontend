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

import base.{ViewFns, ViewSpec}
import models.ocelot.{LabelCache, Labels}
import models.ui._
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components.inset_text

import scala.collection.JavaConverters._

class InsetTextSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.StandardPage("/url", Seq(currencyInput))
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
    val insetText: InsetText = InsetText(Seq(Text("Line1", "Welsh, Line1"), Text("Line2", "Welsh, Line2")))
  }

  private trait WelshTest extends Test {implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))}

  "English Inset Text" must {

    "be rendered as an <div> with govuk classes and <p> elements" in new Test {
      val html: Html = inset_text(insetText)
      val div: Element = getSingleElementByTag(html, "div")

      div.hasClass("govuk-inset-text") shouldBe true

      val lis = div.getElementsByTag("p").asScala.toList
      lis.length shouldBe 2
      lis(0).text shouldBe "Line1"
      lis(1).text shouldBe "Line2"
    }
  }

  "Welsh Inset Text" must {

    "be rendered as an <div> with govuk classes and <p> elements" in new WelshTest {
      val html: Html = inset_text(insetText)
      val div: Element = getSingleElementByTag(html, "div")

      div.hasClass("govuk-inset-text") shouldBe true

      val lis = div.getElementsByTag("p").asScala.toList
      lis.length shouldBe 2
      lis(0).text shouldBe "Welsh, Line1"
      lis(1).text shouldBe "Welsh, Line2"
    }
  }

}
