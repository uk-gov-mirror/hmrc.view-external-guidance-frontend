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

package services

import base.BaseSpec
import models.ocelot.stanzas._
import models.ocelot._
import models.ui.{BulletPointList, HyperLink, PageLink, Text}

class UIBuilderSpec extends BaseSpec with ProcessJson {

  trait Test extends ProcessJson {

    val lang0 = Vector("Some Text","Welsh, Some Text")
    val lang1 = Vector("Some Text1","Welsh, Some Text1")
    val lang2 = Vector("Some Text2","Welsh, Some Text2")
    val lang3 = Vector("Some Text3","Welsh, Some Text3")
    val lang4 = Vector("Some Text4","Welsh, Some Text4")

    val ltxt1 = Text("This is a ","Welsh, This is a ", true)
    val ltxt2 = Text(" followed by "," Welsh, followed by ")
    val ltxt3 = Text(" and nothing"," Welsh, and nothing")
    val link1Txt = Text("A link","Welsh, A link")
    val link2Txt = Text("Another Link","Welsh, Another Link")

    val link1Txt2 = Text("A link at start of phrase","Welsh, A link at start of phrase")
    val link2Txt2 = Text("Another Link at end of phrase","Welsh, Another Link at end of phrase")

    val pageLink1Text = Text("A page link","Welsh, A page link")
    val pageLink2Text = Text("Another page link","Welsh, Another page link")
    val q1 = Vector("Do you agree?", "Welsh, Do you agree?")
    val ans1 = Vector("Yes", "Welsh, Yes")
    val ans2 = Vector("No", "Welsh, Yes")
    val ans3 = Vector("Not sure", "Welsh, Yes")

    val ans1WithHint = Vector("Yes[hint:You agree with the assertion]", "Welsh, Yes[hint:Welsh, You agree with the assertion]")
    val ans2WithHint = Vector("No[hint:You DONT agree with the assertion]", "Welsh, Yes[hint:Welsh, You DONT agree with the assertion]")
    val ans3WithHint = Vector("Not sure[hint:You dont know]", "Welsh, Yes[hint:Welsh, You dont know]")

    val hint1 = Text("You agree with the assertion","Welsh, You agree with the assertion")
    val hint2 = Text("You DONT agree with the assertion","Welsh, You DONT agree with the assertion")
    val hint3 = Text("You dont know","Welsh, You dont know")

    val link1 = HyperLink("https://www.bbc.co.uk", link1Txt, false)
    val link2 = HyperLink("https://www.gov.uk", link2Txt, false)
    val link2_1 = HyperLink("https://www.bbc.co.uk", link1Txt2, false)
    val link2_2 = HyperLink("https://www.gov.uk", link2Txt2, false)
    val link3 = HyperLink("dummy-path/blah", Text(lang4), false)
    val link4 = HyperLink("https://www.bbc.co.uk", Text(lang4), false)

    val pageLink1 = PageLink("dummy-path/next", pageLink1Text)
    val pageLink2 = PageLink("dummy-path", pageLink2Text)

    implicit val urlMap: Map[String, String] = Map("3" -> "dummy-path",
                                                   "4" -> "dummy-path/question",
                                                   "5" -> "dummy-path/blah",
                                                   "6" -> "dummy-path/anotherquestion",
                                                   "34" -> "dummy-path/next")
    val answerDestinations = Seq("4","5","6")
    val answerDestinationUrls = Seq("dummy-path/question","dummy-path/blah","dummy-path/anotherquestion")

    val txtWithLinks = Phrase(
      Vector("[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A link:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link:https://www.gov.uk] Welsh, and nothing")
    )
    val txtWithLinks2 = Phrase(
      Vector("[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:Another Link at end of phrase:https://www.gov.uk]",
      "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link at end of phrase:https://www.gov.uk]")
    )
    val txtWithPageLinks = Phrase(
      Vector("[bold:This is a ][link:A page link:34] followed by [link:Another page link:3] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A page link:34] Welsh, followed by [link:Welsh, Another page link:3] Welsh, and nothing")
    )
    val txtWithAllLinks = Phrase(
      Vector("[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:A page link:34]",
      "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, A page link:34]")
    )

    val linkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(Link(7,"5","",false)), false)
    val hyperLinkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(Link(7,"https://www.bbc.co.uk","",false)), false)
    val embeddedLinkInstructionStanza = Instruction(txtWithLinks, Seq("end"), None, false)
    val embeddedLinkInstructionStanza2 = Instruction(txtWithLinks2, Seq("end"), None, false)
    val embeddedPageLinkInstructionStanza = Instruction(txtWithPageLinks, Seq("end"), None, false)
    val embeddedAllLinkInstructionStanza = Instruction(txtWithAllLinks, Seq("end"), None, false)

    val questionPhrase: Phrase = Phrase(q1)
    val answers = Seq(Phrase(ans1),Phrase(ans2),Phrase(ans3))
    val answersWithHints = Seq(Phrase(ans1WithHint),Phrase(ans2WithHint),Phrase(ans3WithHint))
    val question: models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, false)
    val questionWithAnswerHints: models.ocelot.stanzas.Question = Question(questionPhrase, answersWithHints, answerDestinations, false)

    val initialStanza = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Callout(Title, Phrase(lang0), Seq("3"), false),
      Callout(SubTitle, Phrase(lang1), Seq("4"), false),
      Callout(Lede, Phrase(lang2), Seq("5"), false),
      Instruction(Phrase(lang3), Seq("end"), None, false)
    )

    val stanzasWithQuestion = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Instruction(Phrase(lang3), Seq("3"), None, false),
      question
    )

    val stanzasWithQuestionAndHints = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Instruction(Phrase(lang3), Seq("3"), None, false),
      questionWithAnswerHints
    )

    val questionPage = Page("start","/",stanzasWithQuestion, Seq(""), Nil)
    val questionPageWithHints = Page("start","/",stanzasWithQuestionAndHints, Seq(""), Nil)

    val stanzas = initialStanza ++ Seq(linkInstructionStanza, EndStanza)
    val stanzasWithHyperLink = initialStanza ++ Seq(hyperLinkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedLinks = initialStanza ++ Seq(embeddedLinkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedLinks2 = initialStanza ++ Seq(embeddedLinkInstructionStanza2, EndStanza)
    val stanzasWithEmbeddedPageLinks = initialStanza ++ Seq(embeddedPageLinkInstructionStanza, EndStanza)
    val stanzasWithEmbeddedAllLinks = initialStanza ++ Seq(embeddedAllLinkInstructionStanza, EndStanza)
    val page = Page("start", "/test-page", stanzas, Seq(""), Nil)
    val hyperLinkPage = Page("start", "/test-page", stanzasWithHyperLink, Seq(""), Nil)

    val pageItems = Seq(ltxt1, link1, ltxt2, link2, ltxt3)
    val textItems = Seq(ltxt1, link1, ltxt2, link2, ltxt3)
    val textItems2 = Seq(link2_1, ltxt2, link2_2)
    val pageLinkTextItems = Seq(ltxt1, pageLink1, ltxt2, pageLink2, ltxt3)
    val allLinksTextItems = Seq(link2_1, ltxt2, pageLink1)

    val pageWithEmbeddLinks = page.copy(stanzas = stanzasWithEmbeddedLinks)
    val pageWithEmbeddLinks2 = page.copy(stanzas = stanzasWithEmbeddedLinks2)
    val pageWithEmbeddPageLinks = page.copy(stanzas = stanzasWithEmbeddedPageLinks)
    val pageWithEmbeddAllLinks = page.copy(stanzas = stanzasWithEmbeddedAllLinks)

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]",
                                         "Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    // for multi page testing
    val stanzaPages = PageBuilder.pages(prototypeJson.as[Process]).right.get
    val prototypeUrlMap = stanzaPages.map(p => (p.id, p.url)).toMap

  }

  "UIBuilder" must {

    "convert and Ocelot page into a UI page with the same url" in new Test{

      UIBuilder.fromStanzaPage(page) match {
        case p if p.urlPath == page.url => succeed
        case p => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
      }
    }

    "convert 1st Callout type Title to H1" in new Test{
      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(1) mustBe models.ui.H1(Text(lang0))
    }

    "convert 2nd Callout type SubTitle to H2" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(2) mustBe models.ui.H2(Text(lang1))
    }

    "convert Callout type Lede to lede Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(3) mustBe models.ui.Paragraph(Seq(Text(lang2)), true)
    }

    "convert Simple instruction to Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(4) mustBe models.ui.Paragraph(Seq(Text(lang3)), false)
    }

    "convert Link instruction to Paragraph" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(5) mustBe models.ui.Paragraph(Seq(models.ui.HyperLink("dummy-path/blah", Text(lang4), false)), false)
    }

    "convert page with instruction stanza containing a sequence of Text and HyperLink items" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(textItems, false)
    }

    "convert page with instruction stanza containing a sequence of TextItems beginning and ending with HyperLinks" in new Test{
      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddLinks2)
      uiPage.components(5) mustBe models.ui.Paragraph(textItems2, false)
    }

    "convert page with instruction stanza text containing PageLinks and Text" in new Test{
      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddPageLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(pageLinkTextItems, false)
    }

    "convert a sequence of stanza pages into a map of UI pages by url" in new Test {
      val pageMap = UIBuilder.pages(stanzaPages)

      pageMap.keys.toList.length mustBe stanzaPages.length

      stanzaPages.foreach{ p =>
        pageMap.contains(p.url) mustBe true
      }
    }

    "convert page with instruction stanza text containing PageLinks, HyperLinks and Text" in new Test {
      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddAllLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(allLinksTextItems, false)
    }

    "convert page including a PageLink instruction stanza" in new Test {
      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(5) mustBe models.ui.Paragraph(Seq(link3), false)
    }

    "convert page including a HyperLink instruction stanza" in new Test {
      val uiPage = UIBuilder.fromStanzaPage(hyperLinkPage)
      uiPage.components(5) mustBe models.ui.Paragraph(Seq(link4), false)
    }

    "convert a question page into a Seq of a single Question UI object" in new Test {
      val uiPage = UIBuilder.fromStanzaPage(questionPage)

      uiPage.components.length mustBe 1

      uiPage.components.head match {
        case q:models.ui.Question =>
          q.answers.length mustBe 3

          q.body.length mustBe 2

          q.answers(0) mustBe models.ui.Answer(Text(ans1), None, answerDestinationUrls(0))

          q.answers(1) mustBe models.ui.Answer(Text(ans2), None, answerDestinationUrls(1))

          q.answers(2) mustBe models.ui.Answer(Text(ans3), None, answerDestinationUrls(2))

        case _ => fail("Found non question UIComponent")
      }
    }

    "convert a question page including answer hints into a Seq of a single Question UI object" in new Test {
      val uiPage = UIBuilder.fromStanzaPage(questionPageWithHints)

      uiPage.components.length mustBe 1

      uiPage.components.head match {
        case q:models.ui.Question =>
          q.answers.length mustBe 3

          q.body.length mustBe 2

          q.answers(0) mustBe models.ui.Answer(Text(ans1), Some(hint1), answerDestinationUrls(0))

          q.answers(1) mustBe models.ui.Answer(Text(ans2), Some(hint2), answerDestinationUrls(1))

          q.answers(2) mustBe models.ui.Answer(Text(ans3), Some(hint3), answerDestinationUrls(2))

        case _ => fail("Found non question UIComponent")
      }
    }

    "Process page with a simple instruction group" in new Test {

      val phrase1: Phrase = Phrase( Vector( "My favourite sweets are wine gums", "Fy hoff losin yw deintgig gwin" ) )
      val phrase2: Phrase = Phrase( Vector( "My favourite sweets are humbugs", "Fy hoff losin yw humbugs" ) )

      val instruction1: Instruction = Instruction( phrase1, Seq( "2" ), None, false )
      val instruction2: Instruction = Instruction( phrase2, Seq( "end" ), None, false )

      val instructionGroup: InstructionGroup = InstructionGroup( Seq( instruction1, instruction2 ) )

      val bulletPointListStanzas = Seq(
        ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
        instructionGroup
      )

      val bulletPointListPage = Page("start","/", bulletPointListStanzas, Seq(""), Nil)

      val uiPage = UIBuilder.fromStanzaPage( bulletPointListPage )

      uiPage.components.length mustBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text( "My favourite sweets are", "Fy hoff losin yw", false  )

      val bulletPointOne: Text = Text( "wine gums", "deintgig gwin", false )
      val bulletPointTwo: Text = Text( "humbugs", "humbugs", false )

      uiPage.components.head match {
        case b: BulletPointList => {

          b.leadingText.head mustBe leadingTextItems

          b.listItems.size mustBe 2

          b.listItems.head mustBe Seq( bulletPointOne )
          b.listItems.last mustBe Seq( bulletPointTwo )
        }
        case _ => fail( "Did not find bullet point list")
      }
    }

  }

}
