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
import play.api.i18n.{Messages, MessagesApi}
import play.api.test.FakeRequest
import play.twirl.api.Html
import org.jsoup.Jsoup
import views.html.components.page
import models.ui.{Page,H1,Paragraph,Text}


class PageSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  private trait Test {
    private def injector: Injector = app.injector
    private def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)
    val title = Text("Telling HMRC about extra income",
                     "Tudalen Arddangos Yn Adrodd HMRC am incwm ychwanegol")

    val openingPara = Text("Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.",
                           "Gwiriwch a oes angen i chi ddweud wrth HMRC am arian ychwanegol rydych chi " +
                           "wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo.")

    val simplePage =  Page("root",
                           Seq(
                             H1(title),
                             Paragraph(Seq(openingPara), lede = true)
                           )
                      )

  }

  "Page component" should {
    "generate html containing an H1 and a text only paragraph" in new Test {
      val html: String = page(simplePage).body.trim

      println(html)
    }

  }

}
