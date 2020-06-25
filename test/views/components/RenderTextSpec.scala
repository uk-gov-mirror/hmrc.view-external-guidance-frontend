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
import views.html.components.{h1_heading, h2_heading, h3_heading, paragraph}
import models.ui.{H1, H2, H3, Paragraph, Text, Words}
import org.jsoup.nodes.{Document, Element}
import scala.collection.JavaConverters._

class RenderTextSpec extends WordSpec with Matchers with GuiceOneAppPerSuite {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]
    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))
    val fakeRequest = FakeRequest("GET", "/")

    val boldText = Text(Words("Hello", true), Words("Welsh Hello", true))
    val normalText = Text(Words("Hello", false), Words("Welsh Hello", false))

  }

  trait WelshTest extends Test {
    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))
  }

  "render_text component" should {

    "generate English html containing normal text" in new Test {
      val doc = asDocument(paragraph(Paragraph(normalText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 0
    }

    "generate English html containing an H1" in new Test {
      val doc = asDocument(h1_heading(H1(normalText)))
      val h1 = doc.getElementsByTag("h1")
      h1.size shouldBe 1
    }

    "generate English html containing an H2" in new Test {
      val doc = asDocument(h2_heading(H2(normalText)))
      val h2 = doc.getElementsByTag("h2")
      h2.size shouldBe 1
    }

    "generate English html containing an H3" in new Test {
      val doc = asDocument(h3_heading(H3(normalText)))
      val h3 = doc.getElementsByTag("h3")
      h3.size shouldBe 1
    }

    "generate English html containing a normal text paragraph" in new Test {
      val doc = asDocument(paragraph(Paragraph(boldText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 1
    }

    "generate Welsh html containing normal text" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(normalText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 0
    }

    "generate Welsh html containing an H1" in new WelshTest {
      val doc = asDocument(h1_heading(H1(normalText)))
      val h1 = doc.getElementsByTag("h1")
      h1.size shouldBe 1
    }

    "generate Welsh html containing an H2" in new WelshTest {
      val doc = asDocument(h2_heading(H2(normalText)))
      val h2 = doc.getElementsByTag("h2")
      h2.size shouldBe 1
    }

    "generate Welsh html containing an H3" in new WelshTest {
      val doc = asDocument(h3_heading(H3(normalText)))
      val h3 = doc.getElementsByTag("h3")
      h3.size shouldBe 1
    }

    "generate Welsh html containing a normal text paragraph" in new WelshTest {
      val doc = asDocument(paragraph(Paragraph(boldText)))
      val strong = doc.getElementsByTag("strong")
      strong.size shouldBe 1
    }

  }
}
