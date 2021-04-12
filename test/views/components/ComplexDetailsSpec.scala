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

import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.inject.Injector
import play.api.i18n.{Lang, Messages, MessagesApi}
import core.models.ocelot.{LabelCache, Labels}
import models.PageContext
import models.ui.{ComplexDetails, StandardPage, Text}
import views.html._
import base.{BaseSpec, ViewFns, ViewSpec}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConverters._

class ComplexDetailsSpec extends BaseSpec with ViewFns with ViewSpec with GuiceOneAppPerSuite {

  private trait Test {

    private def injector: Injector = app.injector

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    implicit val labels: Labels = LabelCache()

    // Define example texts and bullet point list text groups

    // Complex details with single bullet point
    val caption: Text = Text("Title")

    val bpl1LeadingText: Text = Text("Choose your favourite sweets")

    val bpl1ListItem1: Text = Text("Wine gums")
    val bpl1ListItem2: Text = Text("Bonbons")
    val bpl1ListItem3: Text = Text("Fruit pastilles")

    val bpl1TextGroup1: Seq[Text] = Seq(
        bpl1LeadingText,
        bpl1ListItem1,
        bpl1ListItem2,
        bpl1ListItem3
      )

    val complexDetails1: ComplexDetails = ComplexDetails(caption, Seq(bpl1TextGroup1))

    val page1: StandardPage = StandardPage("/page-1", Seq(complexDetails1))

    val page1Ctx: PageContext = PageContext(
      page1,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    // Complex details with text and bullet point
    val text1: Text = Text("Text1")

    val complexDetails2: ComplexDetails = ComplexDetails(caption, Seq(Seq(text1), bpl1TextGroup1 ))

    val page2: StandardPage = StandardPage("/page-2", Seq(complexDetails2))

    val page2Ctx: PageContext = PageContext(
      page2,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    // Complex details with bullet point followed by two text items
    val text2: Text = Text("Text2")

    val complexDetails3: ComplexDetails = ComplexDetails(caption, Seq(bpl1TextGroup1, Seq(text1), Seq(text2)))

    val page3: StandardPage = StandardPage("/page-3", Seq(complexDetails3))

    val page3Ctx: PageContext = PageContext(
      page3,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )

    // Complex details with two bullet point lists and three text items
    val bpl2LeadingText: Text = Text("Your favourite holiday destinations are")

    val bpl2ListItem1: Text = Text("Acapulca")
    val bpl2ListItem2: Text = Text("Bognor Regis")

    val bpl2TextGroup2: Seq[Text] = Seq(
      bpl2LeadingText,
      bpl2ListItem1,
      bpl2ListItem2
    )

    val text3: Text = Text("Text3")

    val complexDetails4: ComplexDetails = ComplexDetails(caption, Seq(
        Seq(text1),
        bpl1TextGroup1,
        Seq(text2),
        bpl2TextGroup2,
        Seq(text3)
      )
    )

    val page4: StandardPage = StandardPage("/page-4", Seq(complexDetails4))

    val page4Ctx: PageContext = PageContext(
      page4,
      Seq.empty,
      None,
      "sessionId",
      None,
      Text(),
      "processId",
      "processCode",
      labels
    )
  }

  "Complex details component" must {

    "render a single bullet point list text group" in new Test {

      val doc: Document = asDocument(components.complexDetails(complexDetails1)(messages, page1Ctx))

      val details: Elements = doc.getElementsByTag("details")

      details.size shouldBe 1

      val summaries: Elements = details.first.getElementsByTag("summary")

      summaries.size shouldBe 1

      summaries.text() shouldBe caption.asString

      val divs: Elements = details.first.getElementsByTag("div")

      divs.size shouldBe 1

      val div: Element = divs.first

      val divChildren: Elements = div.children()

      divChildren.first.tag.toString shouldBe "p"
      divChildren.first.text() shouldBe bpl1LeadingText.asString

      divChildren.last.tag.toString shouldBe "ul"

      val listItems: List[Element] = divChildren.last.getElementsByTag("li").asScala.toList

      listItems.size shouldBe 3

      listItems.head.text() shouldBe bpl1ListItem1.asString
      listItems(1).text() shouldBe bpl1ListItem2.asString
      listItems.last.text() shouldBe bpl1ListItem3.asString
    }

    "render a text element followed by a single bullet point list text group" in new Test {

      val doc: Document = asDocument(components.complexDetails(complexDetails2)(messages, page2Ctx))

      val details: Elements = doc.getElementsByTag("details")

      details.size shouldBe 1

      val divs: Elements = details.first.getElementsByTag("div")

      divs.size shouldBe 1

      val children: List[Element] = divs.first.children().asScala.toList

      children.size shouldBe 3

      children.head.tag.toString shouldBe "p"
      children.head.text() shouldBe text1.asString

      children(1).tag.toString shouldBe "p"
      children(1).text() shouldBe bpl1LeadingText.asString

      children.last.tag.toString shouldBe "ul"

      val listItems: List[Element] = children.last.getElementsByTag("li").asScala.toList

      listItems.size shouldBe 3
    }

    "render a bullet point list text group followed by two text items" in new Test {

      val doc: Document = asDocument(components.complexDetails(complexDetails3)(messages, page3Ctx))

      val details: Elements = doc.getElementsByTag("details")

      details.size shouldBe 1

      val divs: Elements = details.first.getElementsByTag("div")

      divs.size shouldBe 1

      val children: List[Element] = divs.first.children().asScala.toList

      children.size shouldBe 4

      children.head.tag.toString shouldBe "p"
      children.head.text shouldBe bpl1LeadingText.asString

      children(1).tag.toString shouldBe "ul"

      val listItems: List[Element] = children(1).getElementsByTag("li").asScala.toList

      listItems.size shouldBe 3

      children(2).tag.toString shouldBe "p"
      children(2).text shouldBe text1.asString

      children.last.tag.toString shouldBe "p"
      children.last.text shouldBe text2.asString
    }

    "render a complex details component with three texts and two bullet point liost groups" in new Test {

      val doc: Document = asDocument(components.complexDetails(complexDetails4)(messages, page4Ctx))

      val details: Elements = doc.getElementsByTag("details")

      details.size shouldBe 1

      val divs: Elements = details.first.getElementsByTag("div")

      divs.size shouldBe 1

      val children: List[Element] = divs.first.children().asScala.toList

      children.size shouldBe 7

      children.head.tag.toString shouldBe "p"
      children.head.text shouldBe text1.asString

      children(1).tag.toString shouldBe "p"
      children(1).text shouldBe bpl1LeadingText.asString

      children(2).tag.toString shouldBe "ul"

      val bpl1ListItems: List[Element] = children(2).getElementsByTag("li").asScala.toList

      bpl1ListItems.size shouldBe 3

      children(3).tag.toString shouldBe "p"
      children(3).text shouldBe text2.asString

      children(four).tag.toString shouldBe "p"
      children(four).text shouldBe bpl2LeadingText.asString

      children(five).tag.toString shouldBe "ul"

      val bpl2ListItems: List[Element] = children(five).getElementsByTag("li").asScala.toList

      bpl2ListItems.size shouldBe 2

      children.last.tag.toString shouldBe "p"
      children.last.text shouldBe text3.asString
    }

  }

}
