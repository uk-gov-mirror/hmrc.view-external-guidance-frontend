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
import org.jsoup.select.Elements
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

        // Test leading text
        val paragraph: Element = getSingleElementByTag( markUp, "p" )

        paragraph.hasClass( "govuk-body") mustBe true

        paragraph.text() mustBe englishLeadingText

        // Test list items
        checkListClasses( markUp )

        val expectedListItems = List( englishBulletPointOne, englishBulletPointTwo, englishBulletPointThree )

        val actualListItems = getMultipleElementsByTag( markUp, "li", 3 ).asScala.toList

        assert( actualListItems.map( _.text ) == expectedListItems, "\nActual bullet point list items do not match those expected" )
      }

      "Render simple bullet point list in Welsh" in new WelshTest {

        val markUp: Html = bullet_point_list( simpleBulletPointList )

        // Test leading text
        val paragraph: Element = getSingleElementByTag( markUp, "p" )

        paragraph.hasClass( "govuk-body") mustBe true

        paragraph.text() mustBe welshLeadingText

        // Test list items
        checkListClasses( markUp )

        val expectedListItems = List( welshBulletPointOne, welshBulletPointTwo, welshBulletPointThree )

        val actualListItems = getMultipleElementsByTag( markUp, "li", 3 ).asScala.toList

        assert( actualListItems.map( _.text ) == expectedListItems, "\nActual bullet point list items do not match those expected" )
      }

    }

  }

  def checkListClasses( markUp: Html ) : Unit = {

    val ulList: Element = getSingleElementByTag( markUp, "ul" )

    ulList.hasClass( "govuk-list" ) mustBe true
    ulList.hasClass( "govuk-list--bullet" ) mustBe true

  }

}
