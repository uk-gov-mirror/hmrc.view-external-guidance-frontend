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
import views.html.components.page
import models.ui.{Page,H1,Paragraph,Text}
import org.jsoup.nodes.Document


class PageSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")


    val title = Text("Telling HMRC about extra income",
                     "Tudalen Arddangos Yn Adrodd HMRC am incwm ychwanegol")

    val openingPara = Text("Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.",
                           "Gwiriwch a oes angen i chi ddweud wrth HMRC am arian ychwanegol rydych chi " +
                           "wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo.")

    val para = Paragraph(Seq(openingPara))
    val simplePage =  Page("root", Seq(para, H1(title)))
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Page component" should {

    "generate English html containing an H1 and a text only paragraph" in new Test {

      val doc = asDocument(page(simplePage))

      val paras = doc.getElementsByTag("p")

      paras.size shouldBe 1
      paras.first.text shouldBe openingPara.english

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.english
    }

    "generate Welsh html containing an H1 and a text only paragraph" in new WelshTest {

      val doc = asDocument(page(simplePage))

      val paras = doc.getElementsByTag("p")

      paras.size shouldBe 1
      paras.first.text shouldBe openingPara.welsh

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.welsh
    }

  }

}
