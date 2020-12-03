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

import base.{BaseSpec, TestConstants}
import play.api.i18n.Lang

class UIComponentsSpec extends BaseSpec with TestConstants {

  val h1English: String = "Heading level 1 text"
  val h1Welsh: String = "Welsh heading level 1 text"

  val h2English: String = "Heading level 2 text"
  val h2Welsh: String = "Welsh heading level 2 text"

  val h3English: String = "Heading level 3 text"
  val h3Welsh: String = "Welsh heading level 3 text"

  val h4English: String = "Heading level 4 text"
  val h4Welsh: String = "Welsh heading level 4 text"

  val h1 = H1(Text(h1English, h1Welsh))
  val h2 = H2(Text(h2English, h2Welsh))
  val h3 = H3(Text(h3English, h3Welsh))
  val h4 = H4(Text(h4English, h4Welsh))

  val engLeadingText: String = "Leading text"
  val welLeadingText: String = "Welsh leading text"

  val engBulletPointOneText = "Bullet point 1"
  val welBulletPointOneText = "Welsh bullet point 1"

  val engBulletPointTwoText = "Bullet point 2"
  val welBulletPointTwoText = "Welsh bullet point 2"

  val englishLang: Lang = Lang("en")
  val welshLang: Lang = Lang("cy")

  "UIComponents" must {

    "return appropriate lang text from Text and Text given Lang welsh" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."

      implicit val lang: Lang = Lang("cy")

      Text(english, welsh).value shouldBe Seq(Words(welsh))
    }

    "return appropriate lang text from Text and Text given Lang english" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      implicit val lang: Lang = Lang("en")

      Text(english, welsh).value shouldBe Seq(Words(english))
    }

    "return appropriate lang text from Text and Text given Lang unknown" in {
      val welsh = "Welsh, Hello my name is ...."
      val english = "Hello my name is ...."
      implicit val lang: Lang = Lang("jp")

      Text(english, welsh).value shouldBe Seq(Words(english))
    }

    "use text components with an overriding implementation of the method toString" in {
      val english = "Example text"
      val welsh = "Welsh example text"

      Text(english, welsh).toString shouldBe s"[$english:$welsh]"
    }

    "support isEmpty within Text" in {
      Text().isEmpty(Lang("en")) shouldBe true
      Text().isEmpty(Lang("cy")) shouldBe true

      Text("", "").isEmpty(Lang("en")) shouldBe false

      Text("", "").english.forall(_.isEmpty) shouldBe true
      Text("", "").welsh.forall(_.isEmpty) shouldBe true
    }

    "Support English and Welsh text in HTML h1 elements" in {

      h1.text.value(englishLang) shouldBe Seq(Words(h1English))
      h1.text.value(welshLang) shouldBe Seq(Words(h1Welsh))
    }

    "Support English and Welsh text in HTML h2 elements" in {

      h2.text.value(englishLang) shouldBe Seq(Words(h2English))
      h2.text.value(welshLang) shouldBe Seq(Words(h2Welsh))
    }

    "Support English and Welsh text in HTML h3 elements" in {

      h3.text.value(englishLang) shouldBe Seq(Words(h3English))
      h3.text.value(welshLang) shouldBe Seq(Words(h3Welsh))
    }

    "Support English and Welsh text in HTML h4 elements" in {

      h4.text.value(englishLang) shouldBe Seq(Words(h4English))
      h4.text.value(welshLang) shouldBe Seq(Words(h4Welsh))
    }

    "Support the identification of heading components in a list of UIComponents" in {

      val uiComponents: List[UIComponent] = List(h1, h2, h3, h4)

      uiComponents.head match {
        case matchedH1: H1 => matchedH1 shouldBe h1
        case _ => fail("H1 Html component not matched")
      }

      uiComponents(1) match {
        case matchedH2: H2 => matchedH2 shouldBe h2
        case _ => fail("H2 Html component not matched")
      }

      uiComponents(2) match {
        case matchedH3: H3 => matchedH3 shouldBe h3
        case _ => fail("H3 Html component not matched")
      }

      uiComponents(3) match {
        case matchedH4: H4 => matchedH4 shouldBe h4
        case _ => fail("H4 Html component not matched")
      }

    }

    "return appropriate language text from bullet point list when lang is english" in {

      val text: Text = Text(engLeadingText, welLeadingText)
      val bulletPointOne: Text = Text(engBulletPointOneText, welBulletPointOneText)
      val bulletPointTwo: Text = Text(engBulletPointTwoText, welBulletPointTwoText)
      val bulletPointList: BulletPointList = BulletPointList(text, Seq(bulletPointOne, bulletPointTwo))

      implicit val lang: Lang = Lang("en")

      bulletPointList.text match {
        case text: Text => text.value shouldBe text.english
        case _ => fail("The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head match {
        case text: Text => text.value shouldBe bulletPointOne.english
        case _ => fail("The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1) match {
        case text: Text => text.value shouldBe bulletPointTwo.english
        case _ => fail("The first text item in the second bullet point is not an instance of the class Text")
      }
    }

    "return appropriate language text from bullet point list when lang is welsh" in {

      val text: Text = Text(engLeadingText, welLeadingText)
      val bulletPointOne: Text = Text(engBulletPointOneText, welBulletPointOneText)
      val bulletPointTwo: Text = Text(engBulletPointTwoText, welBulletPointTwoText)
      val bulletPointList: BulletPointList = BulletPointList(text, Seq(bulletPointOne, bulletPointTwo))

      implicit val lang: Lang = Lang("cy")

      bulletPointList.text match {
        case text: Text => text.value shouldBe text.welsh
        case _ => fail("The first text item in leading text is not an instance of the class Text")
      }

      bulletPointList.listItems.head match {
        case text: Text => text.value shouldBe bulletPointOne.welsh
        case _ => fail("The first text item in the first bullet point is not an instance of the class Text")
      }

      bulletPointList.listItems(1) match {
        case text: Text => text.value shouldBe bulletPointTwo.welsh
        case _ => fail("The first text item in the second bullet point is not an instance of the class Text")
      }
    }

    "support bullet point lists containing both text and embedded links" in {

      // Create bullet point list with both text and embedded links
      val leadingTxt1En: Words = Words("Leading text 1")
      val leadingTxt1Cy: Words = Words("Welsh leading text 1")

      val leadingLinkEn: Link = Link("http://textUrl", "Leading text link", false)
      val leadingLinkCy: Link = Link("http://textUrl", "Welsh leading text link", false)

      val leadingTxt2En: Words = Words("Leading text 2")
      val leadingTxt2Cy: Words = Words("Welsh leading text 2")

      val bp1TxtEn: Words = Words("Bullet point 1 text")
      val bp1TxtCy: Words = Words("Welsh bullet point 1 text")

      val bp1LinkEn: Link = Link("http://bulletPointOneUrl", "Bullet point 1 link", false)
      val bp1LinkCy: Link = Link("http://bulletPointOneUrl", "Welsh bullet point 1 link", false)

      val bp2TxtEn: Words = Words("Bullet point 2 text")
      val bp2TxtCy: Words = Words("Welsh bullet point 2 text")

      val bp2LinkEn: Link = Link("http://bulletPointTwoUrl", "Bullet point 2 link", true)
      val bp2LinkCy: Link = Link("http://bulletPointTwoUrl", "Welsh bullet point 2 link", true)

      val bpListLeadingText: Text =
        Text(Seq(leadingTxt1En, leadingLinkEn, leadingTxt2En), Seq(leadingTxt1Cy, leadingLinkCy, leadingTxt2Cy))

      val bulletPointListItems: Seq[Text] = Seq(
        Text(Seq(bp1TxtEn, bp1LinkEn), Seq(bp1TxtCy, bp1LinkCy)),
        Text(Seq(bp2TxtEn, bp2LinkEn), Seq(bp2TxtCy, bp2LinkCy))
      )

      val bulletPointList: BulletPointList = BulletPointList(bpListLeadingText, bulletPointListItems)

      // Test components of bullet point list
      bulletPointList.text.english(0).toString shouldBe "Leading text 1"
      bulletPointList.text.english(1).toString shouldBe "[link:Leading text link:http://textUrl:false:None]"
      bulletPointList.text.english(2).toString shouldBe "Leading text 2"

      bulletPointList.listItems.head.english(0).toString shouldBe "Bullet point 1 text"
      bulletPointList.listItems.head.english(1).toString shouldBe "[link:Bullet point 1 link:http://bulletPointOneUrl:false:None]"

      bulletPointList.listItems(1).english(0).toString shouldBe "Bullet point 2 text"
      bulletPointList.listItems(1).english(1).toString shouldBe "[link:Bullet point 2 link:http://bulletPointTwoUrl:true:None]"
    }

    "use Link components with an implementation of the toString method for use in debugging" in {

      val englishLinkText = "English link text"
      val welshLinkText = "Welsh link text"

      val destination = "http://my.com/page"

      val linkEn: Link = Link(destination, englishLinkText, false)
      val linkCy: Link = Link(destination, welshLinkText, false)

      linkEn.toString shouldBe s"[link:$englishLinkText:$destination:false:None]"
      linkCy.toString shouldBe s"[link:$welshLinkText:$destination:false:None]"
    }

    "use Button Link components with an implementation of the toString method for use in debugging" in {

      val englishLinkText = "English link text"
      val welshLinkText = "Welsh link text"

      val destination = "http://my.com/page"

      val linkEn: Link = Link(destination, englishLinkText, false, true)
      val linkCy: Link = Link(destination, welshLinkText, false, true)

      linkEn.toString shouldBe s"[button:$englishLinkText:$destination:false:None]"
      linkCy.toString shouldBe s"[button:$welshLinkText:$destination:false:None]"
    }

    "build into a page of text and link paragraph" in {

      val langs1 = Vector("Hello my name is ....", "Welsh, Hello my name is ....")
      val langs2 = Vector(" and today is Wednesday", "Welsh,  and today is Wednesday")
      val langs3 = Vector("Unsure?", "Welsh, Unsure?")

      val txt1En = Words(langs1(0))
      val txt1Cy = Words(langs1(1))
      val txt2En = Words(langs2(0))
      val txt2Cy = Words(langs2(1))

      val link1En = Link("/secondpage", langs3(0))
      val link1Cy = Link("/secondpage", langs3(1))

      val paraWithLink = Paragraph(Text(Seq(txt1En, link1En, txt2En), Seq(txt1Cy, link1Cy, txt2Cy)))
      val components = Seq(paraWithLink)
      val page = Page("/firstpage", components)

      page.components.length shouldBe 1
      page.components.foreach {
        case p: Paragraph => p.text.value(englishLang).length shouldBe 3
        case _ => fail("unknown ParagraphItem")
      }
    }

    "use Link components which correctly support isEmpty" in {
      Link("/secondpage", "").isEmpty shouldBe true
      Link("/secondpage", "Hello").isEmpty shouldBe false
    }

    "use Link components which correctly support toString" in {
      val en: String = "Hello"
      Link("4", en).toString shouldBe s"[link:${en}:4:false:None]"
    }

    "use link components that add a query string marker to the destination when it matches the previous page in the guidance" in {

      val destination: String = "/guidance/test/page-2"

      val link: Link = Link(destination, "See page 2", window = false)

      val backLink: Option[String] = Some(destination)

      link.getDest(backLink) shouldBe s"$destination?$PreviousPageLinkQuery"
    }

    "use link components that do not add a query string marker to the destination when it does not match the previous page" in {

      val destination: String = "/guidance/test/page-5"

      val link: Link = Link(destination, "See page 5", window = false)

      val backLink: Option[String] = Some("/guidance/test/page-3")

      link.getDest(backLink) shouldBe destination
    }

    "use link components that do not add a query string marker to the destination when the previous page is not defined" in {

      val destination: String = "/guidance/test/page-4"

      val link: Link = Link(destination, "See page 4", window = true, asButton = true, hint = Some("Hint"))

      link.getDest(None) shouldBe destination
    }

    "use LabelRef components which correctly support isEmpty" in {
      LabelRef("Blah").isEmpty shouldBe false
    }

    "use LabelRef components which correctly support toString" in {
      LabelRef("BLAH").toString shouldBe s"[label:BLAH:Txt]"
    }

    "use LabelRef components which correctly support toWords" in {
      LabelRef("This is a label").toWords shouldBe List("This", "is", "a", "label")
    }

    "allow creation of Text object containing a simple LabelRef with labelRef() helper" in {
      Text.labelRef("BLAH") shouldBe Text(Seq(LabelRef("BLAH")), Seq(LabelRef("BLAH")))
    }

    "build a complete page" in {

      // Define page title
      val h1Page: H1 = H1(Text("Title text", "Welsh title text"))

      // Define opening paragraph
      val openingParagraphText: Text = Text("Welcome", "Welsh welcome")
      val openingParagraph: Paragraph = Paragraph(openingParagraphText)

      // Define sub-title
      val h2Page: H2 = H2(Text("Subtitle", "Welsh subtitle"))

      // Define second paragraph with embedded hyperlink
      val secondParagraphLeadingText: Text = Text("Opening section of second paragraph", "Welsh opening section of second paragraph")

      val secondEnLink: Link = Link(
        "http://secondParagraphUrl",
        "Second paragraph link text",
        false
      )
      val secondCyLink: Link = Link(
        "http://secondParagraphUrl",
        "Welsh second paragraph link text",
        false
      )

      val link = Text(secondEnLink, secondCyLink)

      val secondParagraphClosingText: Text = Text(
        "Closing section of second paragraph",
        "Welsh closing section of second paragraph"
      )

      val secondParagraph: Paragraph = Paragraph(secondParagraphLeadingText + link + secondParagraphClosingText)

      // Define text of some note
      val h3Page: H3 = H3(Text("Note", "Welsh note"))

      // Define bullet point list
      val text: Text = Text("Leading text for bullet point list", "Welsh leading text for bullet point list")
      val bulletPointOne: Text = Text("Bullet point 1", "Welsh bullet point 1")
      val bulletPointTwo: Text = Text("Bullet point 2", "Welsh bullet point 2")
      val bulletPointThree: Text = Text("Bullet point 3", "Welsh bullet point 3")
      val bp3LinkEn: Link = Link("http://thirdBulletPointUrl", "Third bullet point link", false)
      val bp3LinkCy: Link = Link("http://thirdBulletPointUrl", "Welsh third bullet point link", false)

      val bulletPointList: BulletPointList = BulletPointList(
        text,
        Seq(bulletPointOne, bulletPointTwo, bulletPointThree + Text(bp3LinkEn, bp3LinkCy))
      )

      val components: Seq[UIComponent] = Seq(h1Page, openingParagraph, h2Page, secondParagraph, h3Page, bulletPointList)

      val page: Page = Page("/pageUrl", components)

      page.components.length shouldBe 6

      page.components.head match {
        case matchedH1: H1 => matchedH1 shouldBe h1Page
        case _ => fail("First component in page is not a H1 component")
      }

      page.components(1) match {
        case matchedOpeningParagraph: Paragraph => matchedOpeningParagraph shouldBe openingParagraph
        case _ => fail("Second component in page is not a paragraph")
      }

      page.components(2) match {
        case matchedH2: H2 => matchedH2 shouldBe h2Page
        case _ => fail("Third component in page is not a H2 component")
      }

      page.components(3) match {
        case matchedSecondParagraph: Paragraph => matchedSecondParagraph shouldBe secondParagraph
        case _ => fail("Fourth component in page is not a paragraph")
      }

      page.components(four) match {
        case matchedH3: H3 => matchedH3 shouldBe h3Page
        case _ => fail("Fifth component in page is not a H3 component")
      }

      page.components(five) match {
        case matchedBulletPointList: BulletPointList => matchedBulletPointList shouldBe bulletPointList
        case _ => fail("Sixth component in page is not a bullet point list")
      }
    }

  }

}
