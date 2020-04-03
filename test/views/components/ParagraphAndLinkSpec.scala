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
import models.ui.{Paragraph, Text, Link}
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._

class ParagraphAndLinkSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val paraText1 = Text("Hello", "Welsh Hello")
    val paraText2 = Text("World", "Welsh World")

    val ledePara = Paragraph(paraText1, lede = true)
    val para = Paragraph(paraText1, lede = false)
    val dest1 = "https://www.bbc.co.uk"
    val dest2 = "https://www.bbc.co.uk/news"
    val link1En = Link(dest1, "The BBC", window = true)
    val link2En = Link(dest2, "BBC News", window = false)
    val pageLinkEn = Link(dest2, "BBC News")
    val link1Cy = Link(dest1, "Welsh, The BBC", window = true)
    val link2Cy = Link(dest2, "Welsh, BBC News", window = false)
    val pageLinkCy = Link(dest2, "Welsh, BBC News")

    val link1 = Text(link1En, link1Cy)
    val link2 = Text(link2En, link2Cy)
    val pageLink = Text(pageLinkEn, pageLinkCy)

    val paraWithMultipleLinks = Paragraph(paraText1 + link1 + paraText2 + link2 + pageLink)
  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "Paragraph component" should {

    "generate English html containing a lede text paragraph" in new Test {

      val doc = asDocument(paragraph(ledePara))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.english.head.toString
      paras.first.classNames.toString shouldBe "[govuk-body-l]"
    }

    "generate English html containing a normal text paragraph" in new Test {

      val doc = asDocument(paragraph(para))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.english.head.toString
      paras.first.classNames.toString shouldBe "[govuk-body]"
    }

    "generate English html containing Text and links" in new Test {

      val doc = asDocument(paragraph(paraWithMultipleLinks))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1

      val links = paras.first.getElementsByTag("a").asScala
      links.length shouldBe 3

      val txtNodes = paras.first.textNodes().asScala
      txtNodes.length shouldBe 4
      txtNodes(0).text().trim shouldBe paraText1.value(messages.lang).head.toString
      txtNodes(1).text().trim shouldBe paraText2.value(messages.lang).head.toString

      val link1Attrs = links(0).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link1Attrs.contains("href") shouldBe true
      link1Attrs("href") shouldBe dest1
      link1Attrs.contains("class") shouldBe true
      link1Attrs("class") shouldBe "govuk-link"
      link1Attrs.contains("target") shouldBe true
      link1Attrs("target") shouldBe """"_blank""""

      val link2Attrs = links(1).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link2Attrs.contains("href") shouldBe true
      link2Attrs("href") shouldBe dest2
      link2Attrs.contains("class") shouldBe true
      link2Attrs("class") shouldBe "govuk-link"
      link2Attrs.contains("target") shouldBe false

      val link3Attrs = links(2).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link3Attrs.contains("href") shouldBe true
      link3Attrs("href") shouldBe dest2
      link3Attrs.contains("class") shouldBe true
      link3Attrs("class") shouldBe "govuk-link"
      link3Attrs.contains("target") shouldBe false
    }

    "generate Welsh html containing a lede text paragraph" in new WelshTest {

      val doc = asDocument(paragraph(ledePara))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.welsh.head.toString
      paras.first.classNames.toString shouldBe "[govuk-body-l]"
    }

    "generate Welsh html containing a normal text paragraph" in new WelshTest {

      val doc = asDocument(paragraph(para))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1
      paras.first.text shouldBe paraText1.welsh.head.toString
      paras.first.classNames.toString shouldBe "[govuk-body]"
    }

    "generate Welsh html containing Text and links" in new WelshTest {

      val doc = asDocument(paragraph(paraWithMultipleLinks))
      val paras = doc.getElementsByTag("p")
      paras.size shouldBe 1

      val links = paras.first.getElementsByTag("a").asScala
      links.length shouldBe 3

      val txtNodes = paras.first.textNodes().asScala
      txtNodes.length shouldBe 4
      txtNodes(0).text().trim shouldBe paraText1.value(messages.lang).head.toString
      txtNodes(1).text().trim shouldBe paraText2.value(messages.lang).head.toString

      val link1Attrs = links(0).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link1Attrs.contains("href") shouldBe true
      link1Attrs("href") shouldBe dest1
      link1Attrs.contains("class") shouldBe true
      link1Attrs("class") shouldBe "govuk-link"
      link1Attrs.contains("target") shouldBe true
      link1Attrs("target") shouldBe """"_blank""""

      val link2Attrs = links(1).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link2Attrs.contains("href") shouldBe true
      link2Attrs("href") shouldBe dest2
      link2Attrs.contains("class") shouldBe true
      link2Attrs("class") shouldBe "govuk-link"
      link2Attrs.contains("target") shouldBe false

      val link3Attrs = links(2).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
      link3Attrs.contains("href") shouldBe true
      link3Attrs("href") shouldBe dest2
      link3Attrs.contains("class") shouldBe true
      link3Attrs("class") shouldBe "govuk-link"
      link3Attrs.contains("target") shouldBe false

    }

  }

}
