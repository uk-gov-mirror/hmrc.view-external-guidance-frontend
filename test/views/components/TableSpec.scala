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

class TableSpec extends ViewSpec with ViewFns with GuiceOneAppPerSuite {

  private trait Test {
    implicit val labels: Labels = LabelCache()
    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit val fakeRequest = FakeRequest("GET", "/")
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

    val expectedTable = Table(None,
                              Some(Seq(Th(Text("HELLO","HELLO")), Th(Text("World","World")))),
                              Seq.fill(3)(Seq(Td(Text("HELLO","HELLO")), Td(Text("World","World")))))
    val expectedTableNoHeadings = Table(None,
                                        None,
                                        Seq.fill(3)(Seq(Td(Text("HELLO","HELLO")), Td(Text("World","World")))))
    val expectedTableWithNumericCells = Table(None,
                                              Some(Seq(Th(Text("HELLO","HELLO")), Th(Text("World","World")))),
                                              Seq.fill(3)(Seq(Th(Text("HELLO","HELLO")), Td(Text(LabelRef("Blah", Currency), LabelRef("Blah", Currency))))))
    val expectedTableWithCaption = expectedTable.copy(caption = Some(Text("Caption", "Caption")))

    val currencyInput = models.ui.CurrencyInput(Text(), None, Seq.empty)
    val page = models.ui.InputPage("/url", currencyInput)
    implicit val ctx = models.PageContext(page, "sessionId", None, Text(), "processId", "processCode", labels)
  }

  private trait WelshTest extends Test {implicit override def messages: Messages = messagesApi.preferred(Seq(Lang("cy")))}

  "English Tables" must {

    "Encode all bold initial row as table headings" in new Test {
      val html: Html = components.table(expectedTable)
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.isEmpty shouldBe true
      table.hasClass("govuk-table") shouldBe true
      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTable.headingRow.fold(fail){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode an initial row not all bold as a standard table body row" in new Test {
      val html: Html = components.table(expectedTableNoHeadings)
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.isEmpty shouldBe true
      table.hasClass("govuk-table") shouldBe true
      table.getElementsByTag("thead").asScala.toList.isEmpty shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode all bold initial row as table headings with a Caption" in new Test {
      val html: Html = components.table(expectedTableWithCaption)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      expectedTableWithCaption.caption.fold(fail){caption =>
        val element: Element = table.getElementsByTag("caption").first
        element.hasClass("govuk-table__caption") shouldBe true
        element.text shouldBe caption.asString(messages.lang)
      }

      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTableWithCaption.headingRow.fold(fail){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode table with numeric cells using govuk numeric class" in new Test {
      val html: Html = components.table(expectedTableWithNumericCells)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size

      (rows zip expectedTableWithNumericCells.rows).foreach{
        case (rowElem, tableRow) =>
          val cells = rowElem.children.asScala.toList
          (cells zip tableRow).foreach{
            case (c, td: Td) if td.numeric =>
              c.hasClass("govuk-table__cell--numeric") shouldBe true

            case (c, tc) =>
              c.hasClass("govuk-table__cell--numeric") shouldBe false
          }
      }
    }
  }

  "Welsh Tables" must {

    "Encode all bold initial row as table headings" in new WelshTest {
      val html: Html = components.table(expectedTable)
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.isEmpty shouldBe true
      table.hasClass("govuk-table") shouldBe true
      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTable.headingRow.fold(fail){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode an initial row not all bold as a standard table body row" in new WelshTest {
      val html: Html = components.table(expectedTableNoHeadings)
      val table: Element = getSingleElementByTag(html, "Table")
      table.getElementsByTag("caption").asScala.toList.isEmpty shouldBe true
      table.hasClass("govuk-table") shouldBe true
      table.getElementsByTag("thead").asScala.toList.isEmpty shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode all bold initial row as table headings with a Caption" in new WelshTest {
      val html: Html = components.table(expectedTableWithCaption)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      expectedTableWithCaption.caption.fold(fail){caption =>
        val element: Element = table.getElementsByTag("caption").first
        element.hasClass("govuk-table__caption") shouldBe true
        element.text shouldBe caption.asString(messages.lang)
      }

      val head = table.getElementsByTag("thead").first
      val headings = head.getElementsByTag("th").asScala.toList
      expectedTableWithCaption.headingRow.fold(fail){row =>
        headings.size shouldBe row.size
      }

      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size
    }

    "Encode table with numeric cells using govuk numeric class" in new WelshTest {
      val html: Html = components.table(expectedTableWithNumericCells)
      val table: Element = getSingleElementByTag(html, "Table")
      table.hasClass("govuk-table") shouldBe true
      val body = table.getElementsByTag("tbody").first
      val rows = body.getElementsByTag("tr").asScala.toList

      rows.size shouldBe expectedTable.rows.size

      (rows zip expectedTableWithNumericCells.rows).foreach{
        case (rowElem, tableRow) =>
          val cells = rowElem.children.asScala.toList
          (cells zip tableRow).foreach{
            case (c, td: Td) if td.numeric =>
              c.hasClass("govuk-table__cell--numeric") shouldBe true

            case (c, tc) =>
              c.hasClass("govuk-table__cell--numeric") shouldBe false
          }
      }
    }
  }

}
