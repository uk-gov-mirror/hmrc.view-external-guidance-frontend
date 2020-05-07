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

package views

import config.AppConfig
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatest.{Matchers, WordSpec}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.inject.Injector
import play.api.test.FakeRequest
import play.twirl.api.Html
import views.html.hello_world
import scala.collection.JavaConverters._


class helloWorldSpec extends WordSpec with Matchers with GuiceOneAppPerSuite  {

  def asDocument(html: Html): Document = Jsoup.parse(html.toString)

  trait Test {
    private def injector: Injector = app.injector

    def helloWorld: hello_world = injector.instanceOf[hello_world]

    implicit val fakeRequest = FakeRequest("GET", "/")

    def messagesApi: MessagesApi = injector.instanceOf[MessagesApi]

    implicit def messages: Messages = messagesApi.preferred(Seq(Lang("en")))

    implicit def appconfig: AppConfig = injector.instanceOf[AppConfig]
  }

    "feedback component" should {
      "generate English html containing Text and links" in new Test {

        val doc = asDocument(helloWorld())
        val links = doc.getElementsByTag("a").asScala
        val link1Attrs = links(3).attributes.asScala.toList.map(attr => (attr.getKey, attr.getValue)).toMap
        link1Attrs.contains("href") shouldBe true
        link1Attrs("href") shouldBe appconfig.feedbackUrl
      }
    }

}