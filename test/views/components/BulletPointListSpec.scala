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

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import models.ui._
import models.ocelot.{Labels, LabelCache}
import views.html.components.bullet_point_list
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import base.ViewSpec
import scala.collection.JavaConverters._

class BulletPointListSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {

    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

  }

  private trait WelshTest extends Test {

    implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))

  }

  "Bullet point lists" must {

    "Render a simple bullet point list with text elements only" must {

      val enLeadingText: String = "You can buy"
      val cyLeadingText: String = "Gwallwch brynu"

      val enBpOne: String = "apples"
      val cyBpOne: String = "afalau"

      val enBpTwo: String = "oranges"
      val cyBpTwo: String = "orennau"

      val enBpThree: String = "pears"
      val cyBpThree: String = "gellyg"

      val leadingText: Text = Text(enLeadingText, cyLeadingText)

      val listItems: Seq[Text] = Seq(
        Text(enBpOne, cyBpOne),
        Text(enBpTwo, cyBpTwo),
        Text(enBpThree, cyBpThree)
      )

      val simpleBpList: BulletPointList = BulletPointList(leadingText, listItems)

      "Render simple bullet point list in English" in new Test {

        val markUp: Html = bullet_point_list(simpleBpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        paragraph.text() shouldBe enLeadingText

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(enBpOne, enBpTwo, enBpThree)

        val actualListItems = getMultipleElementsByTag(document, "li", 3).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }

      "Render simple bullet point list in Welsh" in new WelshTest {

        val markUp: Html = bullet_point_list(simpleBpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        paragraph.text() shouldBe cyLeadingText

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(cyBpOne, cyBpTwo, cyBpThree)

        val actualListItems = getMultipleElementsByTag(markUp, "li", 3).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }

    }

    "Render a bullet point list with a link embedded in the leading text" must {

      val enLeading1: String = "To view your options"
      val cyLeading1: String = "Welsh to view your options"

      val linkUrl: String = "http://optionsUrl"
      val linkEnglishText: String = "Click here"
      val linkWelshText: String = "Welsh click here"

      val enLeadingTextPartTwo: String = "Or just give up"
      val cyLeadingTextPartTwo: String = "Welsh or just give up"

      val enBpOneText: String = "Continue to section A"
      val cyBpOneText: String = "Welsh continue to section A"

      val enBpTwoText: String = "Continue to section B"
      val cyBpTwoText: String = "Welsh continue to section B"

      val bpLeadingText: Text = Text(enLeading1, cyLeading1) +
        Text(Link(linkUrl, linkEnglishText), Link(linkUrl, linkWelshText)) +
        Text(enLeadingTextPartTwo, cyLeadingTextPartTwo)

      val bpListItems: Seq[Text] = Seq(Text(enBpOneText, cyBpOneText), Text(enBpTwoText, cyBpTwoText))

      val bpList: BulletPointList = BulletPointList(bpLeadingText, bpListItems)

      "Render bullet point list with embedded link in English" in new Test {

        val markUp: Html = bullet_point_list(bpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNodes = paragraph.textNodes().asScala

        textNodes.length shouldBe 2

        textNodes(0).text().trim shouldBe enLeading1
        textNodes(1).text().trim shouldBe enLeadingTextPartTwo

        val link: Element = getSingleElementByTag(document, "a")

        checkHyperLink(link, linkUrl, linkEnglishText, false)

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(enBpOneText, enBpTwoText)

        val actualListItems = getMultipleElementsByTag(document, "li", 2).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }

      "Render bullet point list with embedded link in Welsh" in new WelshTest {

        val markUp: Html = bullet_point_list(bpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading text
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNodes = paragraph.textNodes().asScala

        textNodes.length shouldBe 2

        textNodes(0).text().trim shouldBe cyLeading1
        textNodes(1).text().trim shouldBe cyLeadingTextPartTwo

        val link: Element = getSingleElementByTag(document, "a")

        checkHyperLink(link, linkUrl, linkWelshText, false)

        // Test list items
        val ul: Element = getSingleElementByTag(document, "ul")

        checkClassesForElement(ul, List("govuk-list", "govuk-list--bullet"))

        val expectedListItems = List(cyBpOneText, cyBpTwoText)

        val actualListItems = getMultipleElementsByTag(document, "li", 2).asScala.toList

        assert(actualListItems.map(_.text) == expectedListItems, "\nActual bullet point list items do not match those expected")
      }

    }

    "Render a bullet point list with links embedded in the list items" must {

      val enLeadingText: String = "Leading text for list"
      val cyLeadingText: String = "Welsh leading text for list"

      val enBpOneText: String = "Bullet point one text"
      val cyBpOneText: String = "Welsh bullet point one text"

      val bpOneLinkUrl: String = "http://bpOneUrl"

      val enBpOneLinkText: String = "Bullet point one link text"
      val cyBpOneLinkText: String = "Welsh bullet point one link text"

      val enBpTwoText: String = "Bullet point two text"
      val cyBpTwoText: String = "Welsh bullet point two text"

      val bpTwoLinkUrl: String = "http://bpTwoUrl"

      val enBpTwoLinkText: String = "Bullet point two link text"
      val cyBpTwoLinkText: String = "Welsh bullet point two link text"

      val bpLeadingText: Text = Text(enLeadingText, cyLeadingText)

      val bpListItems: Seq[Text] = Seq(
        Text(enBpOneText, cyBpOneText) +
          Text(Link(bpOneLinkUrl, enBpOneLinkText), Link(bpOneLinkUrl, cyBpOneLinkText)),
        Text(enBpTwoText, cyBpTwoText) +
          Text(Link(bpTwoLinkUrl, enBpTwoLinkText, true), Link(bpTwoLinkUrl, cyBpTwoLinkText, true))
      )

      val bpList: BulletPointList = BulletPointList(bpLeadingText, bpListItems)

      "Render the bullet point list with embedded links in English" in new Test {

        val markUp: Html = bullet_point_list(bpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading paragraph
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNode = paragraph.textNodes().asScala

        textNode.length shouldBe 1

        textNode(0).text().trim shouldBe enLeadingText

        // Test list items
        val listItems: Elements = getMultipleElementsByTag(document, "li", 2)

        val firstListItem: Element = listItems.first()

        // First list item
        val firstListItemsTextNodes = firstListItem.textNodes().asScala

        firstListItemsTextNodes.length shouldBe 2

        firstListItemsTextNodes(0).text().trim shouldBe enBpOneText

        val firstListItemLinks: Elements = firstListItem.getElementsByTag("a")

        firstListItemLinks.size() shouldBe 1

        checkHyperLink(firstListItemLinks.first, bpOneLinkUrl, enBpOneLinkText, false)

        // Second link item
        val secondListItem = listItems.last

        val secondListItemTextNodes = secondListItem.textNodes().asScala

        secondListItemTextNodes.length shouldBe 2

        secondListItemTextNodes(0).text().trim shouldBe enBpTwoText

        val secondListItemLinks: Elements = secondListItem.getElementsByTag("a")

        secondListItemLinks.size() shouldBe 1

        checkHyperLink(secondListItemLinks.first, bpTwoLinkUrl, enBpTwoLinkText, true)
      }

      "Render the bullet point list with embedded links in Welsh" in new WelshTest {

        val markUp: Html = bullet_point_list(bpList)

        val document: Document = Jsoup.parse(markUp.toString())

        // Test leading paragraph
        val paragraph: Element = getSingleElementByTag(document, "p")

        paragraph.hasClass("govuk-body") shouldBe true

        val textNode = paragraph.textNodes().asScala

        textNode.length shouldBe 1

        textNode(0).text().trim shouldBe cyLeadingText

        // Test list items
        val listItems: Elements = getMultipleElementsByTag(document, "li", 2)

        val firstListItem: Element = listItems.first()

        // First list item
        val firstListItemsTextNodes = firstListItem.textNodes().asScala

        firstListItemsTextNodes.length shouldBe 2

        firstListItemsTextNodes(0).text().trim shouldBe cyBpOneText

        val firstListItemLinks: Elements = firstListItem.getElementsByTag("a")

        firstListItemLinks.size() shouldBe 1

        checkHyperLink(firstListItemLinks.first, bpOneLinkUrl, cyBpOneLinkText, false)

        // Second link item
        val secondListItem = listItems.last

        val secondListItemTextNodes = secondListItem.textNodes().asScala

        secondListItemTextNodes.length shouldBe 2

        secondListItemTextNodes(0).text().trim shouldBe cyBpTwoText

        val secondListItemLinks: Elements = secondListItem.getElementsByTag("a")

        secondListItemLinks.size() shouldBe 1

        checkHyperLink(secondListItemLinks.first, bpTwoLinkUrl, cyBpTwoLinkText, true)
      }

    }

  }

}
