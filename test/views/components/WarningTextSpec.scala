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

import base.{ViewFns, ViewSpec}
import models.ocelot.{LabelCache, Labels}
import models.ui._
import org.jsoup.nodes.Element
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.components. warning_text

import scala.collection.JavaConverters._

class WarningTextSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val request = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(request)

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.StandardPage("/url", Seq(currencyInput))
    implicit val ctx = models.PageContext(page, Seq.empty, None, "sessionId", None, Text(), "processId", "processCode", labels)
    val warningComponent: WarningComponent = WarningComponent(Seq(Text("Line1"), Text("")))
  }

  "Warning Text" must {

    "be rendered as an <div> with govuk classes" in new Test {
      val html: Html = warning_text(warningComponent)
      val div: Element = getSingleElementByTag(html, "div")

      div.hasClass("govuk-warning-text") shouldBe true

      val lis = div.getElementsByClass("govuk-warning-text__assistive").asScala
      lis.length shouldBe 1
      lis.head.text shouldBe "Warning"

      val warntxt = div.getElementsByClass("govuk-warning-text__text").asScala
      warntxt.length shouldBe 1
      warntxt.head.text shouldBe "Warning Line1"
    }
  }
}
