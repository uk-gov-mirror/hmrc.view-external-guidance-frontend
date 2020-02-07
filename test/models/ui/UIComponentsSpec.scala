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

  val four: Int = 4
  val five: Int = 5
  val six: Int = 6

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

    "use text components with an overriding implementation of the method toString" in {
      val english = "Example text"
      val welsh = "Welsh example text"

      Text( english, welsh ).toString mustBe english
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

    "return appropriate language text from bullet point list when lang is english" in {

      val leadingText: RichText = RichText( Seq( Text( engLeadingText, welLeadingText ) ) )

      val bulletPointOne: RichText = RichText( Seq( Text( engBulletPointOneText, welBulletPointOneText ) ) )

      val bulletPointTwo: RichText = RichText( Seq ( Text( engBulletPointTwoText, welBulletPointTwoText ) ) )

      val bulletPointList: BulletPointList = BulletPointList( leadingText, Seq( bulletPointOne, bulletPointTwo ) )

      implicit val lang: Lang = Lang( "en" )

      bulletPointList.leadingText.items.head match {
        case text: Text => text.value mustBe engLeadingText
        case _ => fail( "The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head.items.head match {
        case text: Text => text.value mustBe engBulletPointOneText
        case _ => fail( "The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1).items.head match {
        case text: Text => text.value mustBe engBulletPointTwoText
        case _ => fail( "The first text item in the second bullet point is not an instance of the class Text")
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

    "support bullet point lists containing both text and embedded links" in {

      // Create bullet point list with both text and embedded links
      val leadingTextFirstTextComponent: Text = Text( "Leading text 1", "Welsh leading text 1" )
      val leadingTextHyperLink: HyperLink = HyperLink(
        "http://leadingTextUrl",
        Text( "Leading text link", "Welsh leading text link" ),
        false )
      val leadingTextSecondTextComponent: Text = Text( "Leading text 2", "Welsh leading text 2" )

      val bulletPointOneText: Text = Text( "Bullet point 1 text", "Welsh bullet point 1 text" )
      val bulletPointOneHyperLink: HyperLink = HyperLink(
        "http://bulletPointOneUrl",
        Text( "Bullet point 1 link", "Welsh bullet point 1 link"),
        false )

      val bulletPointTwoText: Text = Text( "Bullet point 2 text", "Welsh bullet point 2 text" )
      val bulletPointTwoHyperLink: HyperLink = HyperLink(
        "http://bulletPointTwoUrl",
        Text( "Bullet point 2 link", "Welsh bullet point 2 link" ),
        true )

      val bulletPointListLeadingText: RichText = RichText(
        Seq( leadingTextFirstTextComponent, leadingTextHyperLink, leadingTextSecondTextComponent )
      )

      val bulletPointListItems: Seq[RichText] = Seq(
        RichText( Seq( bulletPointOneText, bulletPointOneHyperLink )),
        RichText( Seq( bulletPointTwoText, bulletPointTwoHyperLink ) )
      )

      val bulletPointList: BulletPointList = BulletPointList( bulletPointListLeadingText, bulletPointListItems )

      // Test components of bullet point list
      bulletPointList.leadingText.items.head.toString mustBe "Leading text 1"
      bulletPointList.leadingText.items(1).toString mustBe "[link:Leading text link:http://leadingTextUrl:false]"
      bulletPointList.leadingText.items(2).toString mustBe "Leading text 2"

      bulletPointList.listItems.head.items.head.toString mustBe "Bullet point 1 text"
      bulletPointList.listItems.head.items(1).toString mustBe "[link:Bullet point 1 link:http://bulletPointOneUrl:false]"

      bulletPointList.listItems(1).items.head.toString mustBe "Bullet point 2 text"
      bulletPointList.listItems(1).items(1).toString mustBe "[link:Bullet point 2 link:http://bulletPointTwoUrl:true]"
    }

    "use HyperLink components with an implementation of the toString method for use in debugging" in {

      val englishLinkText = "English link text"
      val welshLinkText = "Welsh link text"

      val destination = "http://my.com/page"

      val hyperLink: HyperLink = HyperLink( destination, Text( englishLinkText, welshLinkText ), false )

      hyperLink.toString mustBe s"[link:$englishLinkText:$destination:false]"
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

    "build a complete page" in {

      // Define page title
      val h1: H1 = H1( "Title text", "Welsh title text" )

      // Define opening paragraph
      val openingParagraphText: Text = Text( "Welcome", "Welsh welcome" )
      val openingParagraph: Paragraph = Paragraph( RichText( Seq( openingParagraphText ) ) )

      // Define sub-title
      val h2: H2 = H2( "Subtitle", "Welsh subtitle" )

      // Define second paragraph with embedded hyperlink
      val secondParagraphLeadingText: Text = Text(
        "Opening section of second paragraph",
      " Welsh opening section of second paragraph" )

      val secondParagraphHyperLink: HyperLink = HyperLink(
        "http://secondParagraphUrl",
        Text( "Second paragraph link text", "Welsh second paragraph link text" ),
        false
      )

      val secondParagraphClosingText: Text = Text(
        "Closing section of second paragraph",
        "Welsh closing section of second paragraph"
      )

      val secondParagraph: Paragraph = Paragraph( RichText( Seq(
        secondParagraphLeadingText,
        secondParagraphHyperLink,
        secondParagraphClosingText
      )))

      // Define text of some note
      val h3: H3 = H3( "Note", "Welsh note" )

      // Define bullet point list
      val leadingText: Text = Text( "Leading text for bullet point list", "Welsh leading text for bullet point list" )
      val bulletPointOne: Text = Text( "Bullet point 1", "Welsh bullet point 1" )
      val bulletPointTwo: Text = Text( "Bullet point 2", "Welsh bullet point 2" )
      val bulletPointThree: Text = Text( "Bullet point 3", "Welsh bullet point 3" )
      val bulletPointThreeHyperLink: HyperLink = HyperLink(
        "http://thirdBulletPointUrl",
        Text( "Third bullet point link", "Welsh third bullet point link" ),
        false )

      val bulletPointList: BulletPointList = BulletPointList(
        RichText( Seq( leadingText ) ),
        Seq(
          RichText( Seq( bulletPointOne ) ),
            RichText( Seq( bulletPointTwo ) ),
              RichText( Seq( bulletPointThree, bulletPointThreeHyperLink ) )
            )
      )

      // Define question
      val answer1 = Answer(
        Text( "I pay tax at the higher rate", "Welsh I pay tax at the higher rate"),
        Some(Text( "You are relatively wealthy", "Welsh You are relatively weathly" ) ),
          "/higherTaxRateUrl")
      val answer2 = Answer(
        Text( "I do not pay tax at the higher rate", "Welsh I do not pay tax at the higher rate" ),
        Some(Text( "You are not relatively wealthy", "Welsh you are not relatively wealthy") ),
        "/lowerRateTaxUrl")

      val answers = AnswerGroup(Seq(answer1, answer2), false)
      val question = Question(Text("How much tax do you pay?", "Welsh How much tax do you pay?"), None, answers)

      val components: Seq[UIComponent] = Seq(
        h1,
        openingParagraph,
        h2,
        secondParagraph,
        h3,
        bulletPointList,
        question )

      val page: Page = Page( "/pageUrl", components )

      page.components.length mustBe 7

      page.components.head match {
        case matchedH1: H1 => matchedH1 mustBe h1
        case _ => fail( "First component in page is not a H1 component" )
      }

      page.components(1) match {
        case matchedOpeningParagraph: Paragraph => matchedOpeningParagraph mustBe openingParagraph
        case _ => fail( "Second component in page is not a paragraph" )
      }

      page.components(2) match {
        case matchedH2: H2 => matchedH2 mustBe h2
        case _ => fail( "Third component in page is not a H2 component" )
      }

      page.components(3) match {
        case matchedSecondParagraph: Paragraph => matchedSecondParagraph mustBe secondParagraph
        case _ => fail( "Fourth component in page is not a paragraph" )
      }

      page.components( four ) match {
        case matchedH3: H3 => matchedH3 mustBe h3
        case _ => fail( "Fifth component in page is not a H3 component" )
      }

      page.components( five ) match {
        case matchedBulletPointList: BulletPointList => matchedBulletPointList mustBe bulletPointList
        case _ => fail( "Sixth component in page is not a bullet point list" )
      }

      page.components( six ) match {
        case matchedQuestion: Question => matchedQuestion mustBe question
        case _ => fail( "Seventh component in page is not a question" )
      }
    }

  }
  
}
