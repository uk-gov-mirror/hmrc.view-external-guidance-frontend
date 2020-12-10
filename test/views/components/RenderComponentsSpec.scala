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

import org.jsoup.nodes.Element

import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest

import play.twirl.api.Html

import models.ui._
import models.ocelot.{Labels, LabelCache}
import views.html._
import scala.collection.JavaConverters._
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import base.{ViewFns, ViewSpec}

class RenderComponentsSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val expectedTable = Table(Text("HELLO","HELLO"),
                              Seq(Text("First","First"), Text("Second","Second")),
                              Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("World","World"))))

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.InputPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  private trait WelshTest extends Test {implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))}

  "English render_components" must {

    "Encode table missing caption, with initial row/cell as table caption" in new Test {
      val html: Html = components.render_components(Seq(expectedTable))
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.headOption.fold(fail){ caption =>
        caption.text shouldBe "HELLO"

        table.hasClass("govuk-table") shouldBe true

        val body = table.getElementsByTag("tbody").first
        val rows = body.getElementsByTag("tr").asScala.toList

        rows.size shouldBe expectedTable.rows.size
      }
    }

  }

  "Welsh render_components" must {

    "Encode table missing caption, with initial row/cell as table caption" in new WelshTest {
      val html: Html = components.render_components(Seq(expectedTable))
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.headOption.fold(fail){ caption =>
        caption.text shouldBe "HELLO"

        table.hasClass("govuk-table") shouldBe true

        val body = table.getElementsByTag("tbody").first
        val rows = body.getElementsByTag("tr").asScala.toList

        rows.size shouldBe expectedTable.rows.size
      }
    }
  }
}
