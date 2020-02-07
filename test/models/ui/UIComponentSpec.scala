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

package models.ui

import play.api.i18n.Lang

import base.BaseSpec

class UIComponentSpec extends BaseSpec {

  val h1English: String = "Heading level 1 text"
  val h1Welsh: String = "Welsh heading level 1 text"

  val h2English: String = "Heading level 2 text"
  val h2Welsh: String = "Welsh heading level 2 text"

  val h3English: String = "Heading level 3 text"
  val h3Welsh: String = "Welsh heading level 3 text"

  val h1 = H1( h1English, h1Welsh )
  val h2 = H2( h2English, h2Welsh )
  val h3 = H3( h3English, h3Welsh )

  val engLeadingText: String = "Leading text"
  val welLeadingText: String = "Welsh leading text"

  val engBulletPointOneText = "Bullet point 1"
  val welBulletPointOneText = "Welsh bullet point 1"

  val engBulletPointTwoText = "Bullet point 2"
  val welBulletPointTwoText = "Welsh bullet point 2"

  val englishLang: Lang = Lang( "en" )
  val welshLang: Lang = Lang( "cy" )

  "UI components" must {

    "Support English and Welsh text in HTML h1 elements" in {

      h1.text.value(englishLang) mustBe h1English
      h1.text.value(welshLang) mustBe h1Welsh
    }

    "Support English and Welsh text in HTML h2 elements" in {

      h2.text.value(englishLang) mustBe h2English
      h2.text.value(welshLang) mustBe h2Welsh
    }

    "Support English and Welsh text in HTML h3 elements" in {

      h3.text.value(englishLang) mustBe h3English
      h3.text.value(welshLang) mustBe h3Welsh
    }

    "Support the identification of heading components in a list of UIComponents" in {

      val uiComponents: List[UIComponent] = List(h1, h2, h3)

      uiComponents.head match {
        case matchedH1: H1 => matchedH1 mustBe h1
        case _ => fail("H1 Html component not matched")
      }

      uiComponents(1) match {
        case matchedH2: H2 => matchedH2 mustBe h2
        case _ => fail("H2 Html component not matched")
      }

      uiComponents(2) match {
        case matchedH3: H3 => matchedH3 mustBe h3
        case _ => fail("H3 Html component not matched")
      }

    }

    "return appropriate language text from bullet point list when lang is english" in {

      val leadingText: RichText = RichText(Seq(Text(engLeadingText, welLeadingText)))

      val bulletPointOne: RichText = RichText(Seq(Text(engBulletPointOneText, welBulletPointOneText)))

      val bulletPointTwo: RichText = RichText(Seq(Text(engBulletPointTwoText, welBulletPointTwoText)))

      val bulletPointList: BulletPointList = BulletPointList(leadingText, Seq(bulletPointOne, bulletPointTwo))

      implicit val lang: Lang = Lang("en")

      bulletPointList.leadingText.items.head match {
        case text: Text => text.value mustBe engLeadingText
        case _ => fail("The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head.items.head match {
        case text: Text => text.value mustBe engBulletPointOneText
        case _ => fail("The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1).items.head match {
        case text: Text => text.value mustBe engBulletPointTwoText
        case _ => fail("The first text item in the second bullet point is not an instance of the class Text")
      }
    }

    "return appropriate language text from bullet point list when lang is welsh" in {

      val leadingText: RichText = RichText( Seq( Text( engLeadingText, welLeadingText ) ) )

      val bulletPointOne: RichText = RichText( Seq( Text( engBulletPointOneText, welBulletPointOneText ) ) )

      val bulletPointTwo: RichText = RichText( Seq ( Text( engBulletPointTwoText, welBulletPointTwoText ) ) )

      val bulletPointList: BulletPointList = BulletPointList( leadingText, Seq( bulletPointOne, bulletPointTwo ) )

      implicit val lang: Lang = Lang( "cy" )

      bulletPointList.leadingText.items.head match {
        case text: Text => text.value mustBe welLeadingText
        case _ => fail( "The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head.items.head match {
        case text: Text => text.value mustBe welBulletPointOneText
        case _ => fail( "The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1).items.head match {
        case text: Text => text.value mustBe welBulletPointTwoText
        case _ => fail( "The first text item in the second bullet point is not an instance of the class Text")
      }
    }


  }

}
