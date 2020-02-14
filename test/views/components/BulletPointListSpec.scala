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
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import models.ui._
import views.html.components.bullet_point_list
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import base.ViewSpec
import scala.collection.JavaConverters._

class BulletPointListSpec extends ViewSpec with GuiceOneAppPerSuite {

  private trait Test {

    private def injector: Injector = app.injector
    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    val fakeRequest = FakeRequest( "GET", "/" )
    implicit def messages: Messages = messagesApi.preferred(fakeRequest)

  }

  private trait WelshTest extends Test {

    implicit override def messages: Messages = messagesApi.preferred( Seq( Lang( "cy" ) ) )

  }

  "Bullet point lists" must {

    "Render a simple bullet point list with text elements only" must {

      val englishLeadingText: String = "You can buy"
      val welshLeadingText: String = "Gwallwch brynu"

      val englishBulletPointOne: String = "apples"
      val welshBulletPointOne: String = "afalau"

      val englishBulletPointTwo: String = "oranges"
      val welshBulletPointTwo: String = "orennau"

      val englishBulletPointThree: String = "pears"
      val welshBulletPointThree: String = "gellyg"

      val leadingText: Seq[TextItem] = Seq( Text( englishLeadingText, welshLeadingText ) )

      val listItems: Seq[Seq[TextItem]] = Seq(
        Seq( Text( englishBulletPointOne, welshBulletPointOne ) ),
        Seq( Text( englishBulletPointTwo, welshBulletPointTwo ) ),
        Seq( Text( englishBulletPointThree, welshBulletPointThree ) )
      )

      val simpleBulletPointList: BulletPointList = BulletPointList( leadingText, listItems )

      "Render simple bullet point list in English" in new Test {

        val markUp: Html = bullet_point_list( simpleBulletPointList )

        val document: Document = Jsoup.parse( markUp.toString() )

        // Test leading text
        val paragraph: Element = getSingleElementByTag( document, "p" )

        paragraph.hasClass( "govuk-body" ) mustBe true

        paragraph.text() mustBe englishLeadingText

        // Test list items
        val ul: Element = getSingleElementByTag( document, "ul" )

        checkClassesForElement( ul, List( "govuk-list", "govuk-list--bullet" ) )

        val expectedListItems = List( englishBulletPointOne, englishBulletPointTwo, englishBulletPointThree )

        val actualListItems = getMultipleElementsByTag( document, "li", 3 ).asScala.toList

        assert( actualListItems.map( _.text ) == expectedListItems, "\nActual bullet point list items do not match those expected" )
      }

      "Render simple bullet point list in Welsh" in new WelshTest {

        val markUp: Html = bullet_point_list( simpleBulletPointList )

        val document: Document = Jsoup.parse( markUp.toString() )

        // Test leading text
        val paragraph: Element = getSingleElementByTag( document, "p" )

        paragraph.hasClass( "govuk-body") mustBe true

        paragraph.text() mustBe welshLeadingText

        // Test list items
        val ul: Element = getSingleElementByTag( document, "ul" )

        checkClassesForElement( ul, List( "govuk-list", "govuk-list--bullet" ) )

        val expectedListItems = List( welshBulletPointOne, welshBulletPointTwo, welshBulletPointThree )

        val actualListItems = getMultipleElementsByTag( markUp, "li", 3 ).asScala.toList

        assert( actualListItems.map( _.text ) == expectedListItems, "\nActual bullet point list items do not match those expected" )
      }

    }

    "Render a bullet point list with a link embedded in the leading text" must {

      val englishLeadingTextPartOne: String = "To view your options"
      val welshLeadingTextPartOne: String = "Welsh to view your options"

      val linkUrl: String = "http://optionsUrl"
      val linkEnglishText: String = "Click here"
      val linkWelshText: String = "Welsh click here"

      val englishLeadingTextPartTwo: String= "Or just give up"
      val welshLeadingTextPartTwo: String = "Welsh or just give up"

      val englishBulletPointOneText: String = "Continue to section A"
      val welshBulletPointOneText: String = "Welsh continue to section A"

      val englishBulletPointTwoText: String = "Continue to section B"
      val welshBulletPointTwoText: String = "Welsh continue to section B"

      val bulletPointLeadingText: Seq[TextItem] = Seq(
        Text( englishLeadingTextPartOne, welshLeadingTextPartOne ),
        HyperLink( linkUrl, Text( linkEnglishText, linkWelshText ) ),
        Text( englishLeadingTextPartTwo, welshLeadingTextPartTwo )
      )

      val bulletPointListItems: Seq[Seq[TextItem]] = Seq(
        Seq( Text( englishBulletPointOneText, welshBulletPointOneText ) ),
        Seq( Text( englishBulletPointTwoText, welshBulletPointTwoText ) )
      )

      val bulletPointList: BulletPointList = BulletPointList( bulletPointLeadingText, bulletPointListItems )

      "Render bullet point list with embedded link in English" in {


      }


    }

  }

}
