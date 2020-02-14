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

import scala.collection.JavaConverters._

import org.scalatest.{MustMatchers, WordSpec}

trait ViewSpec extends WordSpec with MustMatchers {

  def getSingleElementByTag( markUp: Html, htmlTag: String ) : Element = {

    val document: Document = Jsoup.parse( markUp.toString() )

    getSingleElementByTag( document, htmlTag )
  }

  def getMultipleElementsByTag( markUp: Html, htmlTag: String, expectedNumberOfElements: Int ) : Elements = {

    val document: Document = Jsoup.parse( markUp.toString() )

    getMultipleElementsByTag( document, htmlTag, expectedNumberOfElements )
  }

  def getSingleElementByTag( document: Document, htmlTag: String ) : Element = {

    val elements: Elements = document.getElementsByTag( htmlTag )

    elements.size() mustBe 1

    elements.first()
  }

  def getMultipleElementsByTag( document: Document, htmlTag: String, expectedNumberOfElements: Int ) : Elements = {

    val elements: Elements = document.getElementsByTag( htmlTag )

    elements.size() mustBe expectedNumberOfElements

    elements
  }

  def checkClassesForElement( element: Element, expectedClasses: List[String] ) : Unit = {

    for( expectedClass <- expectedClasses ) {
      assert( element.hasClass( expectedClass ), "\n Class " + expectedClass + " not found on element" )
    }

  }

  def checkHyperLink( link: Element, destination: String, text: String, targetBlank: Boolean ) : Unit = {

    val linkAttrs: Map[String, String] = link.attributes.asScala.toList.map(
      attr => ( attr.getKey, attr.getValue )
    ).toMap

    linkAttrs.contains( "href" ) mustBe true
    linkAttrs( "href" ) mustBe destination

    linkAttrs.contains( "class" ) mustBe true
    linkAttrs( "class" ) mustBe "govuk-link"

    if( targetBlank ) {

      linkAttrs.contains( "target" ) mustBe true
      linkAttrs( "target" ) mustBe """"_blank""""

    }
    else
    {
       linkAttrs.contains( "target" ) mustBe false
    }

    val textNodes = link.textNodes().asScala

    textNodes.size mustBe 1

    textNodes(0).text().trim mustBe text

  }

}
