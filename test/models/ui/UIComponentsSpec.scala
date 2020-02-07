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

import base.BaseSpec
import play.api.i18n.Lang

class UIComponentsSpec extends BaseSpec {

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

  "UIComponents" must {

    "return appropriate lang text from Text and Text given Lang welsh" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."

      implicit val lang: Lang = Lang("cy")

      Text(english, welsh).value mustBe welsh
    }

    "return appropriate lang text from Text and Text given Lang english" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      implicit val lang: Lang = Lang("en")

      Text(english, welsh).value mustBe english
    }

    "return appropriate lang text from Text and Text given Lang unknown" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      implicit val lang: Lang = Lang("jp")

      Text(english, welsh).value mustBe english
    }

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

    "build into a page of text only paragraph and question" in {

      implicit val lang: Lang = Lang("en")

      val langs1 = Vector("Hello my name is ....","Welsh, Hello my name is ....")
      val langs4 = Vector("Not bad", "Welsh, Not bad")
      val langs5 = Vector("you think this is not bad", "Welsh, you think this is not bad")
      val langs6 = Vector("ok","Welsh, ok")
      val langs7 = Vector("you think this is not bad", "Wels,you think this is not bad")
      val langs8 = Vector("What do you think of this example","Welsh, What do you think of this example")

      val txt1 = Text(langs1(0), langs1(1))
      val para = Paragraph(RichText(Seq(txt1)))

      val answer1 = Answer(Text(langs4(0), langs4(1)), Some(Text(langs5(0), langs5(1))), "/firstpage/notbad")
      val answer2 = Answer(Text(langs6(0), langs6(1)), Some(Text(langs7(0), langs7(1))), "/firstpage/notbad")
      val answers = AnswerGroup(Seq(answer1, answer2), false)
      val question = Question(Text(langs8(0), langs8(1)), None, answers)

      val components = Seq(para, question)
      val page = Page("/firstpage", components)

      page.components.length mustBe 2

      page.components.foreach{
        case p:Paragraph => p.txt.items.length mustBe 1
        case q: Question => q.answerGroup.answers.length mustBe 2
        case _ => fail("unknown ParagraphItem")
      }
    }

    "build into a page of text and link paragraph" in {

      implicit val lang: Lang = Lang("en")

      val langs1 = Vector("Hello my name is ....","Welsh, Hello my name is ....")
      val langs2 = Vector(" and today is Wednesday","Welsh,  and today is Wednesday")
      val langs3 = Vector("Unsure?", "Welsh, Unsure?")
      val langs4 = Vector("Not bad", "Welsh, Not bad")
      val langs5 = Vector("you think this is not bad", "Welsh, you think this is not bad")
      val langs6 = Vector("ok","Welsh, ok")
      val langs7 = Vector("you think this is not bad", "Wels,you think this is not bad")
      val langs8 = Vector("What do you think of this example","Welsh, What do you think of this example")

      val txt1 = Text(langs1(0), langs1(1))
      val txt2 = Text(langs2(0), langs2(1))
      val link1 = HyperLink("/secondpage",Text(langs3(0), langs3(1)))

      val paraWithLink = Paragraph(RichText(Seq(txt1,link1,txt2)))

      val components = Seq(paraWithLink)
      val page = Page("/firstpage", components)

      page.components.length mustBe 1
      page.components.foreach{
        case p:Paragraph => p.txt.items.length mustBe 3
        case _ => fail("unknown ParagraphItem")
      }
    }

  }
}
