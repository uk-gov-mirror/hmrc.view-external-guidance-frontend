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
import views.html.standard_page
import models.ui.{BulletPointList, Page, H1, Paragraph, Text, StandardPage}
import org.jsoup.nodes.Document
import scala.collection.JavaConverters._

class PageSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val standardPageView = app.injector.instanceOf[views.html.standard_page]
    val title = Text("Telling HMRC about extra income", "Tudalen Arddangos Yn Adrodd HMRC am incwm ychwanegol")

    val openingPara = Text(
      "Check if you need to tell HMRC about extra money you’ve made by selling goods or services, or renting land or property.",
      "Gwiriwch a oes angen i chi ddweud wrth HMRC am arian ychwanegol rydych chi " +
        "wedi'i wneud trwy werthu nwyddau neu wasanaethau, neu rentu tir neu eiddo."
    )

    val bulletPointLeadingText = Text("For example:", "Er enghraifft:")

    val bulletPointOne = Text("selling items online or face to face", "gwerthu eitemau ar-lein neu wyneb yn wyneb")

    val bulletPointTwo =
      Text("selling freelance services (such as gardening or babysitting)", "gwerthu gwasanaethau ar eu liwt eu hunain (fel garddio neu warchod plant)")
    val bulletPointThree = Text("hiring out personal equipment (such as power tools)", "llogi offer personol (fel offer pŵer)")

    val para = Paragraph(openingPara)
    val bulletPointList = BulletPointList(bulletPointLeadingText, Seq(bulletPointOne, bulletPointTwo, bulletPointThree))

    val simplePage = StandardPage("root", Seq(para, H1(title), bulletPointList))
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Page component" should {

    "generate English html containing an H1, a text only paragraph and a test only bullet point list" in new Test {

      val doc = asDocument(standardPageView(simplePage, "/")(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.english.head.toString

      val paras = doc.getElementsByTag("p")

      paras.size shouldBe 4

      val firstPara = paras.eq(2)
      firstPara.first.text shouldBe openingPara.value(messages.lang).head.toString

      val secondPara = paras.eq(3)
      secondPara.first.text shouldBe bulletPointLeadingText.english.head.toString

      val actualListItems = doc.getElementsByTag("li").asScala.toList.dropRight(1)
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] =
        List(bulletPointOne.english.head.toString, bulletPointTwo.english.head.toString, bulletPointThree.english.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

    "generate Welsh html containing an H1 and a text only paragraph" in new WelshTest {

      val doc = asDocument(standardPageView(simplePage, "/")(fakeRequest, messages))

      val h1s = doc.getElementsByTag("h1")
      h1s.size shouldBe 1
      h1s.first.text shouldBe title.welsh.head.toString

      val paras = doc.getElementsByTag("p")

      paras.size shouldBe 4

      val firstPara = paras.eq(2)
      firstPara.first.text shouldBe openingPara.welsh.head.toString

      val secondPara = paras.eq(3)
      secondPara.first.text shouldBe bulletPointLeadingText.welsh.head.toString

      val actualListItems = doc.getElementsByTag("li").asScala.toList.dropRight(1)
      actualListItems.size shouldBe 3

      val expectedListItems: List[String] = List(bulletPointOne.welsh.head.toString, bulletPointTwo.welsh.head.toString, bulletPointThree.welsh.head.toString)

      assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
    }

  }

}
