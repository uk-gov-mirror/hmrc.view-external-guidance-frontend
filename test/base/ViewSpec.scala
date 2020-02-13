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

package base

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import play.twirl.api.Html

import org.scalatest.{MustMatchers, WordSpec}

trait ViewSpec extends WordSpec with MustMatchers {

  def getBodyFragmentElementByTag( markUp: Html, htmlTag: String ) : Element = {

    val document: Document = Jsoup.parseBodyFragment( s"<div>${markUp.toString()}</div>" )

    val headers: Elements = document.getElementsByTag( htmlTag )

    headers.size() mustBe 1

    headers.first()
  }

}
