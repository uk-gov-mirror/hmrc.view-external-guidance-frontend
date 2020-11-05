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
import models.ui.{BulletPointList, Link, H1, H3, H4, Paragraph, Text, Words, FormData, InputPage, QuestionPage}
import models.ui.SummaryList
import play.api.data.FormError

class UIBuilderSpec extends BaseSpec with ProcessJson {

  trait QuestionTest {

    implicit val urlMap: Map[String, String] =
      Map(
        Process.StartStanzaId -> "/blah",
        "3" -> "dummy-path",
        "4" -> "dummy-path/question",
        "5" -> "dummy-path/blah",
        "6" -> "dummy-path/anotherquestion",
        "34" -> "dummy-path/next"
      )
    val answerDestinations = Seq("4", "5", "6")
    val questionPhrase: Phrase = Phrase(Vector("Some Text", "Welsh, Some Text"))
    val questionHintString = "A hint!!"
    val questionWithHintPhrase: Phrase = Phrase(Vector(s"Some Text[hint:${questionHintString}]", s"Welsh, Some Text[hint:${questionHintString}]"))

    val answers =
      Seq(Phrase(Vector("Some Text", "Welsh, Some Text")), Phrase(Vector("Some Text", "Welsh, Some Text")), Phrase(Vector("Some Text", "Welsh, Some Text")))
    val question: models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)

    val stanzas = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Callout(Error, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("3"), false)),
      KeyedStanza("3", Callout(Section, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("31"), false)),
      KeyedStanza("31", Callout(SubSection, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("4"), false)),
      KeyedStanza("4", Instruction(Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("end"), None, false))
    )

    val page = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", Question(questionPhrase, answers, answerDestinations, None, false)), Seq.empty)

    val pageWithQuestionHint =
      Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", Question(questionWithHintPhrase, answers, answerDestinations, None, false)), Seq.empty)

    val uiBuilder: UIBuilder = new UIBuilder()

    val four: Int = 4
  }

  "UIBulider Question processing" must {

    "Ignore Error Callouts when there are no errors" in new QuestionTest {
      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, None)(urlMap) match {
        case s: QuestionPage if s.question.errorMsgs.isEmpty => succeed
        case s: QuestionPage => fail("No error messages should be included on page")
        case _ => fail("Should return QuestionPage")
      }
    }

    "Include Error messages when there are errors" in new QuestionTest {
      val formError = new FormError("test-page", List("error.required"))
      val formData = Some(FormData("test-page", Map(), List(formError)))

      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, formData)(urlMap) match {
        case s: QuestionPage if s.question.errorMsgs.isEmpty => fail("No error messages found on page")
        case s: QuestionPage => succeed
        case _ => fail("Should return QuestionPage")
      }
    }

    "Maintain order of components within a Question" in new QuestionTest {
      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, None) match {
        case q: QuestionPage =>
          q.question.body(0) match {
            case h: H3 => succeed
            case _ => fail("Ordering of question body components not maintained")
          }
          q.question.body(1) match {
            case h: H4 => succeed
            case _ => fail("Ordering of question body components not maintained")
          }
          q.question.body(2) match {
            case h: Paragraph => succeed
            case _ => fail("Ordering of question body components not maintained")
          }

        case _ => fail("Page should be a Question page")
      }

    }

    "Include a question hint appended to the question text" in new QuestionTest {
      uiBuilder.buildPage(pageWithQuestionHint.url, pageWithQuestionHint.stanzas.collect{case s: VisualStanza => s})(urlMap) match {
        case s: QuestionPage if s.question.hint == Some(Text(questionHintString, questionHintString)) => succeed
        case s: QuestionPage => fail("No hint found within Question")
        case _ => fail("Should return QuestionPage")
      }
    }

  }

  trait Test extends ProcessJson {

    val lang0 = Vector("Some Text", "Welsh, Some Text")
    val lang1 = Vector("Some Text1", "Welsh, Some Text1")
    val lang2 = Vector("Some Text2", "Welsh, Some Text2")
    val lang3 = Vector("Some Text3", "Welsh, Some Text3")
    val lang4 = Vector("Some Text4", "Welsh, Some Text4")
    val lang5 = Vector("Some Text5", "Welsh, Some Text5")

    val ltxt1 = Text(Words("This is a ", true), Words("Welsh, This is a ", true))
    val ltxt2 = Text(" followed by ", " Welsh, followed by ")
    val ltxt3 = Text(" and nothing", " Welsh, and nothing")
    val link1TxtEn = "A link"
    val link1TxtCy = "Welsh, A link"
    val link2TxtEn = "Another Link"
    val link2TxtCy = "Welsh, Another Link"
    val link2StartEn = "Back to beginning"
    val link2StartCy = "Back to beginning"
    val link1Txt2En = "A link at start of phrase"
    val link1Txt2Cy = "Welsh, A link at start of phrase"
    val link2Txt2En = "Another Link at end of phrase"
    val link2Txt2Cy = "Welsh, Another Link at end of phrase"

    val pageLink1TextEn = "A page link"
    val pageLink1TextCy = "Welsh, A page link"
    val pageLink2TextEn = "Another page link"
    val pageLink2TextCy = "Welsh, Another page link"
    val q1 = Vector("Do you agree?", "Welsh, Do you agree?")
    val ans1 = Vector("Yes", "Welsh, Yes")
    val ans2 = Vector("No", "Welsh, Yes")
    val ans3 = Vector("Not sure", "Welsh, Yes")

    val ans1WithHint = Vector("Yes[hint:You agree with the assertion]", "Welsh, Yes[hint:Welsh, You agree with the assertion]")
    val ans2WithHint = Vector("No[hint:You DONT agree with the assertion]", "Welsh, Yes[hint:Welsh, You DONT agree with the assertion]")
    val ans3WithHint = Vector("Not sure[hint:You dont know]", "Welsh, Yes[hint:Welsh, You dont know]")

    val hint1 = Text("You agree with the assertion", "Welsh, You agree with the assertion")
    val hint2 = Text("You DONT agree with the assertion", "Welsh, You DONT agree with the assertion")
    val hint3 = Text("You dont know", "Welsh, You dont know")

    val link1En = Link("https://www.bbc.co.uk", link1TxtEn)
    val link2En = Link("https://www.gov.uk", link2TxtEn)
    val link2_1En = Link("https://www.bbc.co.uk", link1Txt2En)
    val link2_2En = Link("https://www.gov.uk", link2Txt2En)
    val link3En = Link("dummy-path/blah", lang4(0))
    val link4En = Link("https://www.bbc.co.uk", lang4(0))

    val pageLink1En = Link("dummy-path/next", pageLink1TextEn)
    val pageLink2En = Link("dummy-path", pageLink2TextEn)

    val startLinkEn = Link("/blah", link2StartEn)
    val startLinkCy = Link("/blah", link2StartCy)

    val link1Cy = Link("https://www.bbc.co.uk", link1TxtCy)
    val link2Cy = Link("https://www.gov.uk", link2TxtCy)
    val link2_1Cy = Link("https://www.bbc.co.uk", link1Txt2Cy)
    val link2_2Cy = Link("https://www.gov.uk", link2Txt2Cy)
    val link3Cy = Link("dummy-path/blah", lang4(1))
    val link4Cy = Link("https://www.bbc.co.uk", lang4(1))

    val pageLink1Cy = Link("dummy-path/next", pageLink1TextCy)
    val pageLink2Cy = Link("dummy-path", pageLink2TextCy)

    implicit val urlMap: Map[String, String] =
      Map(
        Process.StartStanzaId -> "/blah",
        "3" -> "dummy-path",
        "4" -> "dummy-path/question",
        "5" -> "dummy-path/blah",
        "6" -> "dummy-path/anotherquestion",
        "34" -> "dummy-path/next"
      )
    val answerDestinations = Seq("4", "5", "6")

    val txtWithLinks = Phrase(
      Vector(
        "[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
        "[bold:Welsh, This is a ][link:Welsh, A link:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link:https://www.gov.uk] Welsh, and nothing"
      )
    )

    val txtWithLinks2 = Phrase(
      Vector(
        "[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:Another Link at end of phrase:https://www.gov.uk]",
        "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link at end of phrase:https://www.gov.uk]"
      )
    )

    val txtWithPageLinks = Phrase(
      Vector(
        "[bold:This is a ][link:A page link:34] followed by [link:Another page link:3] and nothing",
        "[bold:Welsh, This is a ][link:Welsh, A page link:34] Welsh, followed by [link:Welsh, Another page link:3] Welsh, and nothing"
      )
    )

    val txtWithAllLinks = Phrase(
      Vector(
        "[link:A link at start of phrase:https://www.bbc.co.uk] followed by [link:A page link:34][link:Back to beginning:start]",
        "[link:Welsh, A link at start of phrase:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, A page link:34][link:Back to beginning:start]"
      )
    )

    val linkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(models.ocelot.Link(7, "5", "", false)), false)
    val hyperLinkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(models.ocelot.Link(7, "https://www.bbc.co.uk", "", false)), false)
    val embeddedLinkInstructionStanza = Instruction(txtWithLinks, Seq("end"), None, false)
    val embeddedLinkInstructionStanza2 = Instruction(txtWithLinks2, Seq("end"), None, false)
    val embeddedPageLinkInstructionStanza = Instruction(txtWithPageLinks, Seq("end"), None, false)
    val embeddedAllLinkInstructionStanza = Instruction(txtWithAllLinks, Seq("end"), None, false)
    val embeddedSubsectionCalloutStanza = Callout(SubSection, Phrase(lang5), Seq("3"), false)
    val importantCalloutStanza = Callout(Important, Phrase(lang0), Seq("3"), false)
    val questionPhrase: Phrase = Phrase(q1)
    val answers = Seq(Phrase(ans1), Phrase(ans2), Phrase(ans3))
    val answersWithHints = Seq(Phrase(ans1WithHint), Phrase(ans2WithHint), Phrase(ans3WithHint))
    val question: models.ocelot.stanzas.Question = Question(questionPhrase, answers, answerDestinations, None, false)
    val questionWithAnswerHints: models.ocelot.stanzas.Question = Question(questionPhrase, answersWithHints, answerDestinations, None, false)

    val initialStanza = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", Callout(Title, Phrase(lang0), Seq("3"), false)),
      KeyedStanza("3", Callout(SubTitle, Phrase(lang1), Seq("4"), false)),
      KeyedStanza("4", Callout(Lede, Phrase(lang2), Seq("5"), false)),
      KeyedStanza("5", Instruction(Phrase(lang3), Seq("end"), None, false)),
    )

    val stanzasWithQuestion = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", Instruction(Phrase(lang3), Seq("3"), None, false)),
      KeyedStanza("3", question)
    )

    val stanzasWithQuestionAndHints = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Instruction(Phrase(lang2), Seq("2"), None, false)),
      KeyedStanza("2", Instruction(Phrase(lang3), Seq("3"), None, false)),
      KeyedStanza("3", questionWithAnswerHints)
    )

    val questionPage = Page(Process.StartStanzaId, "/blah", stanzasWithQuestion, Seq.empty)

    val questionPageWithHints = Page(Process.StartStanzaId, "/blah", stanzasWithQuestionAndHints, Seq.empty)

    val stanzas: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("1", linkInstructionStanza), KeyedStanza("2", importantCalloutStanza), KeyedStanza("3", EndStanza))
    val stanzasWithHyperLink: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", hyperLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedLinks2: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedLinkInstructionStanza2), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedPageLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedPageLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedAllLinks: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("6", embeddedAllLinkInstructionStanza), KeyedStanza("7", EndStanza))
    val stanzasWithEmbeddedSubsection: Seq[KeyedStanza] = initialStanza ++ Seq(KeyedStanza("1", embeddedSubsectionCalloutStanza), KeyedStanza("2", EndStanza))

    val page: Page = Page(Process.StartStanzaId, "/test-page", stanzas, Seq.empty)

    val hyperLinkPage: Page = Page(Process.StartStanzaId, "/test-page", stanzasWithHyperLink, Seq.empty)

    val pageWithH4 = Page(Process.StartStanzaId, "/test-page", stanzas, Seq.empty)

    val textItems = ltxt1 +
      Text(link1En, link1Cy) +
      ltxt2 +
      Text(link2En, link2Cy) +
      ltxt3
    val textItems2 = Text(link2_1En, link2_1Cy) + ltxt2 + Text(link2_2En, link2_2Cy)

    val pageLinkTextItems = ltxt1 +
      Text(pageLink1En, pageLink1Cy) +
      ltxt2 +
      Text(pageLink2En, pageLink2Cy) +
      ltxt3
    val allLinksTextItems = Text(link2_1En, link2_1Cy) + ltxt2 + Text(pageLink1En, pageLink1Cy) + Text(startLinkEn, startLinkCy)

    val pageWithEmbeddLinks: Page = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedLinks, Seq.empty)

    val pageWithEmbeddLinks2 = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedLinks2, Seq.empty)

    val pageWithEmbeddPageLinks = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedPageLinks, Seq.empty)

    val pageWithEmbeddAllLinks = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedAllLinks, Seq.empty)

    val pageWithEmbeddH4 = Page(Process.StartStanzaId, "/test-page", stanzasWithEmbeddedSubsection, Seq.empty)

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]", "Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    // for multi page testing
    val pageBuilder: PageBuilder = new PageBuilder()
    val stanzaPages = pageBuilder.pages(prototypeJson.as[Process]).right.get
    val prototypeUrlMap = stanzaPages.map(p => (p.id, p.url)).toMap

    // Create pages for extra income V6 process
    val extraIncomeStanzaPages = pageBuilder.pages(prototypeExtraIncomeV6Json.as[Process]).right.get
    val extraIncomeUrlMap = extraIncomeStanzaPages.map(p =>(p.id, p.url)).toMap

    // Define instance of class to be used in tests
    val uiBuilder: UIBuilder = new UIBuilder()

    val four: Int = 4
    val five: Int = 5
  }

  trait SLTest extends Test {
    val rows = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), Phrase()), Seq()))
    val rowsComplete = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), Phrase(Vector("Blah", "Blah"))), Seq()))
    val sparseRows = Seq(rows(0), Row(Seq(Phrase(Vector("HELLO", "HELLO"))), Seq()), rows(2))
    val phraseWithLinkAndHint = Phrase(Vector("[link:Change[hint:HELLO]:3]", "[link:Change[hint:HELLO]:3]"))
    val rowsWithLinkAndHint = Seq.fill(3)(Row(Seq(Phrase(Vector("HELLO", "HELLO")), Phrase(Vector("World", "World")), phraseWithLinkAndHint), Seq()))
    val sparseRowsWithLinkAndHint = Seq(rowsWithLinkAndHint(0), Row(Seq(Phrase(Vector("HELLO", "HELLO"))), Seq()), rowsWithLinkAndHint(2))

    val dlRows = Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("World","World"), Text("","")))
    val expectedDl = SummaryList(dlRows)
    val dlRowsComplete = Seq.fill(3)(Seq(Text("HELLO","HELLO"), Text("World","World"), Text("Blah","Blah")))
    val expectedDlComplete = SummaryList(dlRowsComplete)
    val sparseDlRows = Seq(dlRows(0), Seq(Text("HELLO","HELLO"), Text("",""), Text("","")), dlRows(2))
    val expectedDlSparse = SummaryList(sparseDlRows)
    val dlRowsWithLinkAndHint = Seq.fill(3)(Seq(Text("HELLO","HELLO"),
                                                               Text("World","World"),
                                                               Text.link("dummy-path",Vector("Change", "Change"), false, false, Some(Vector("HELLO", "HELLO")))))
    val expectedDLWithLinkAndHint = SummaryList(dlRowsWithLinkAndHint)
  }

  "UIBuilder" must {

    // "convert a RowGroup with three simple phrase column into a SummaryList" in new SLTest {
    //   val p = uiBuilder.buildPage("/start", Seq(RowGroup(Seq("2"), rows, rows.head.stack)))
    //   p.components.head match {
    //     case dl: SummaryList =>
    //       dl.rows.zipWithIndex.foreach{
    //         case (r, idx) =>
    //           r shouldBe expectedDl.rows(idx)
    //       }
    //     case x => fail(s"Found $x")
    //   }

    // }

    // "convert a RowGroup with three columns into a SummaryList, all columns complete" in new SLTest {
    //   val p = uiBuilder.buildPage("/start", Seq(RowGroup(Seq("2"), rowsComplete, rowsComplete.head.stack)))
    //   p.components.head match {
    //     case dl: SummaryList =>
    //       dl.rows.zipWithIndex.foreach{
    //         case (r, idx) =>
    //           r shouldBe expectedDlComplete.rows(idx)
    //       }
    //     case x => fail(s"Found $x")
    //   }

    // }

    "convert a RowGroup with three sparse columns including a link and hint into a SummaryList" in new SLTest {
      val p = uiBuilder.buildPage("/start", Seq(Callout(Title, Phrase("Heading", "Heading"), Seq.empty, false),
                                                RowGroup(Seq("2"), sparseRowsWithLinkAndHint, true)))
      p.components match {
        case Seq((h: H1), (sl: SummaryList)) => succeed
        case x => fail(s"Found $x")
      }

    }

    "convert a RowGroup with three columns including a link and hint into a SummaryList" in new SLTest {
      val p = uiBuilder.buildPage("/start", Seq(Callout(Title, Phrase("Heading", "Heading"), Seq.empty, false),
                                                RowGroup(Seq("2"), rowsWithLinkAndHint, true)))
      p.components match {
        case Seq((h: H1), (sl: SummaryList)) => succeed
        case x => fail(s"Found $x")
      }

    }

    "convert and Ocelot page into a UI page with the same url" in new Test {

      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}) match {
        case p if p.urlPath == page.url => succeed
        case p => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
      }
    }

    "convert 1st Callout type Title to H1" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(1) shouldBe models.ui.H1(Text(lang0))
    }

    "convert 2nd Callout type SubTitle to H2" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(2) shouldBe models.ui.H2(Text(lang1))
    }

    "convert Callout type Lede to lede Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(3) shouldBe models.ui.Paragraph(Text(lang2), true)
    }

    "convert Callout type Important to an ErrorMsg" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(6) shouldBe models.ui.ErrorMsg("ID", Text(lang0))
    }

    "convert Simple instruction to Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(four) shouldBe models.ui.Paragraph(Text(lang3), false)
    }

    "convert Link instruction to Paragraph" in new Test {

      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      val en = Link("dummy-path/blah", lang4(0))
      val cy = Link("dummy-path/blah", lang4(1))
      uiPage.components(five) shouldBe models.ui.Paragraph(Text(en, cy))
    }

    "convert page with instruction stanza containing a sequence of Text and Link items" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddLinks.url, pageWithEmbeddLinks.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(five) shouldBe models.ui.Paragraph(textItems, false)
    }

    "convert page with instruction stanza containing a sequence of TextItems beginning and ending with HyperLinks" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddLinks2.url, pageWithEmbeddLinks2.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(5) shouldBe models.ui.Paragraph(textItems2, false)
    }

    "convert page with instruction stanza text containing PageLinks and Text" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddPageLinks.url, pageWithEmbeddPageLinks.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(5) shouldBe models.ui.Paragraph(pageLinkTextItems, false)
    }

    "convert Callout type SubSection to H4" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddH4.url, pageWithEmbeddH4.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(five) shouldBe models.ui.H4(Text(lang5))
    }

    "convert page with instruction stanza text containing PageLinks, HyperLinks and Text" in new Test {
      val uiPage = uiBuilder.buildPage(pageWithEmbeddAllLinks.url, pageWithEmbeddAllLinks.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(five) shouldBe models.ui.Paragraph(allLinksTextItems, false)
    }

    "convert page including a PageLink instruction stanza" in new Test {
      val uiPage = uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(five) shouldBe models.ui.Paragraph(Text(link3En, link3Cy), false)
    }

    "convert page including a Link instruction stanza" in new Test {
      val uiPage = uiBuilder.buildPage(hyperLinkPage.url, hyperLinkPage.stanzas.collect{case s: VisualStanza => s})
      uiPage.components(five) shouldBe models.ui.Paragraph(Text(link4En, link4Cy), false)
    }

    "convert a question page into a Seq of a single Question UI object" in new Test {
      val uiPage = uiBuilder.buildPage(questionPage.url, questionPage.stanzas.collect{case s: VisualStanza => s})

      uiPage.components.length shouldBe 1

      uiPage.components.head match {
        case q: models.ui.Question =>
          q.answers.length shouldBe 3

          q.body.length shouldBe 2

          q.answers.head shouldBe models.ui.Answer(Text(ans1), None)

          q.answers(1) shouldBe models.ui.Answer(Text(ans2), None)

          q.answers(2) shouldBe models.ui.Answer(Text(ans3), None)

        case _ => fail("Found non question UIComponent")
      }
    }

    "convert a question page including answer hints into a Seq of a single Question UI object" in new Test {
      val uiPage = uiBuilder.buildPage(questionPageWithHints.url, questionPageWithHints.stanzas.collect{case s: VisualStanza => s})

      uiPage.components.length shouldBe 1

      uiPage.components.head match {
        case q: models.ui.Question =>
          q.answers.length shouldBe 3

          q.body.length shouldBe 2

          q.answers.head shouldBe models.ui.Answer(Text(ans1), Some(hint1))

          q.answers(1) shouldBe models.ui.Answer(Text(ans2), Some(hint2))

          q.answers(2) shouldBe models.ui.Answer(Text(ans3), Some(hint3))

        case _ => fail("Found non question UIComponent")
      }
    }

    "Process page with a simple instruction group" in new Test {

      val phrase1: Phrase = Phrase(Vector("My favourite sweets are wine gums", "Fy hoff losin yw deintgig gwin"))
      val phrase2: Phrase = Phrase(Vector("My favourite sweets are humbugs", "Fy hoff losin yw humbugs"))

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", instructionGroup)
      )

      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.collect{case s: VisualStanza => s})

      uiPage.components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text(Words("My favourite sweets are"), Words("Fy hoff losin yw"))

      val bulletPointOne: Text = Text("wine gums", "deintgig gwin")
      val bulletPointTwo: Text = Text("humbugs", "humbugs")

      uiPage.components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 2

          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process page with a simple instruction group from prototypeJson" in new Test {

      val phrase1: Phrase = Phrase(
        Vector(
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: selling goods or services (trading)",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Gyllid a Thollau EM am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: gwerthu nwyddau neu wasanaethau (masnachu)"
        )
      )
      val phrase2: Phrase = Phrase(
        Vector(
          "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are: renting land or property",
          "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Gyllid a Thollau EM am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi: rhentu tir neu eiddo"
        )
      )

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, true)
      val instruction2: Instruction = Instruction(phrase2, Seq("end"), None, false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction1, instruction2))

      val bulletPointListStanzas = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", instructionGroup)
      )

      val bulletPointListPage = Page(Process.StartStanzaId, "/blah", bulletPointListStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.collect{case s: VisualStanza => s})

      uiPage.components.length shouldBe 1

      // Check contents of bullet point list
      val leadingTextItems: Text = Text(
        "In some circumstances, you do not have to tell HMRC about extra income you've made. In each tax year you can earn up to £11,000, tax free, if you are:",
        "Mewn rhai amgylchiadau, nid oes rhaid i chi ddweud wrth Gyllid a Thollau EM am incwm ychwanegol rydych wedi'i wneud. Ymhob blwyddyn dreth gallwch ennill hyd at £ 11,000, yn ddi-dreth, os ydych chi:"
      )

      val bulletPointOne: Text = Text("selling goods or services (trading)", "gwerthu nwyddau neu wasanaethau (masnachu)")
      val bulletPointTwo: Text = Text("renting land or property", "rhentu tir neu eiddo")

      uiPage.components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 2

          b.listItems.head shouldBe bulletPointOne
          b.listItems.last shouldBe bulletPointTwo
        case _ => fail("Did not find bullet point list")
      }
    }

    "Process complex page with both instruction groups and single instructions" in new Test {

      val phrase1: Phrase = Phrase(Vector("Going to the market", "Mynd i'r farchnad"))
      val phrase2: Phrase = Phrase(Vector("Fruit and Vegetables", "Ffrwythau a llysiau"))
      val phrase3: Phrase = Phrase(Vector("Vegetables", "Llysiau"))
      val phrase4: Phrase = Phrase(Vector("What you can buy in our lovely vegetable market", "Beth allwch chi ei brynu yn ein marchnad llysiau hyfryd"))
      val phrase5: Phrase = Phrase(Vector("Today we have special parsnips for sale", "Heddiw mae gennym bananas arbennig ar werth"))
      val phrase6: Phrase = Phrase(Vector("Today we have special purple carrots for sale", "Heddiw mae gennym foron porffor arbennig ar werth"))
      val phrase7: Phrase = Phrase(Vector("Today we have special brussels sprouts for sale", "Heddiw mae gennym ysgewyll cregyn gleision arbennig ar werth"))
      val phrase8: Phrase = Phrase(Vector("Thank you", "Diolch"))

      val titleCallout: Callout = Callout(Title, phrase1, Seq("1"), false)
      val instruction1: Instruction = Instruction(phrase2, Seq("2"), None, false)
      val subTitleCallout: Callout = Callout(SubTitle, phrase3, Seq("3"), false)
      val instruction2: Instruction = Instruction(phrase4, Seq("4"), None, false)

      val instructionGroupInstruction1: Instruction = Instruction(phrase5, Seq("5"), None, true)
      val instructionGroupInstruction2: Instruction = Instruction(phrase6, Seq("6"), None, false)
      val instructionGroupInstruction3: Instruction = Instruction(phrase7, Seq("7"), None, false)

      val instructionGroup: InstructionGroup = InstructionGroup(Seq(instructionGroupInstruction1, instructionGroupInstruction2, instructionGroupInstruction3))

      val instruction3: Instruction = Instruction(phrase8, Seq("8"), None, false)

      // Build sequence of stanzas
      val stanzaSeq = Seq(
        KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
        KeyedStanza("1", titleCallout),
        KeyedStanza("2", instruction1),
        KeyedStanza("3", subTitleCallout),
        KeyedStanza("4", instruction2),
        KeyedStanza("5", instructionGroup),
        KeyedStanza("6", instruction3)
      )

      val complexPage = Page(Process.StartStanzaId, "/blah", stanzaSeq, Seq.empty)

      val complexUiPage = uiBuilder.buildPage(complexPage.url, complexPage.stanzas.collect{case s: VisualStanza => s})

      complexUiPage.components.size shouldBe 6

      // Check contents of bullet point list
      val leadingTextItems: Text = Text("Today we have special", "Heddiw mae gennym")

      val bulletPointOne: Text = Text("parsnips for sale", "bananas arbennig ar werth")
      val bulletPointTwo: Text = Text("purple carrots for sale", "foron porffor arbennig ar werth")
      val bulletPointThree: Text = Text("brussels sprouts for sale", "ysgewyll cregyn gleision arbennig ar werth")

      complexUiPage.components(four) match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 3

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems.last shouldBe bulletPointThree
        case _ => fail("Did not find bullet point list")
      }

      val finalParagraph: Paragraph = Paragraph(Text("Thank you", "Diolch"))

      complexUiPage.components(five) match {
        case p: Paragraph =>
          p shouldBe finalParagraph
        case _ => fail("The last components is not an instruction")
      }
    }

    "Process page with multiple line bullet point list" in new Test {

      val phrase1: Phrase = Phrase(Vector("You must have a tea bag", "Rhaid i chi gael bag te"))
      val phrase2: Phrase = Phrase(Vector("You must have a cup", "Rhaid i chi gael cwpan"))
      val phrase3: Phrase = Phrase(Vector("You must have a teaspoon", "Rhaid i chi gael llwy de"))
      val phrase4: Phrase = Phrase(Vector("You must have water", "Rhaid i chi gael dŵr"))
      val phrase5: Phrase = Phrase(Vector("You must have an electric kettle", "Rhaid bod gennych chi degell trydan"))
      val phrase6: Phrase = Phrase(Vector("You must have an electricity supply", "Rhaid bod gennych gyflenwad trydan"))

      val instruction1: Instruction = Instruction(phrase1, Seq("2"), None, stack = true)
      val instruction2: Instruction = Instruction(phrase2, Seq("3"), None, stack = true)
      val instruction3: Instruction = Instruction(phrase3, Seq("4"), None, stack = true)
      val instruction4: Instruction = Instruction(phrase4, Seq("5"), None, stack = true)
      val instruction5: Instruction = Instruction(phrase5, Seq("6"), None, stack = true)
      val instruction6: Instruction = Instruction(phrase6, Seq("end"), None, stack = true)

      val instructionGroup: InstructionGroup = InstructionGroup(
        Seq(
          instruction1,
          instruction2,
          instruction3,
          instruction4,
          instruction5,
          instruction6
        )
      )

      val bulletPointStanzas = Seq(KeyedStanza("start", PageStanza("/page-1", Seq("1"), false)),
                                   KeyedStanza("1", instructionGroup))

      val bulletPointListPage = Page(Process.StartStanzaId, "/page-1", bulletPointStanzas, Seq.empty)

      val uiPage = uiBuilder.buildPage(bulletPointListPage.url, bulletPointListPage.stanzas.collect{case s: VisualStanza => s})

      uiPage.components.length shouldBe 1

      val leadingTextItems: Text = Text("You must have", "Rhaid")

      val bulletPointOne: Text = Text("a tea bag", "i chi gael bag te")
      val bulletPointTwo: Text = Text("a cup", "i chi gael cwpan")
      val bulletPointThree: Text = Text("a teaspoon", "i chi gael llwy de")
      val bulletPointFour: Text = Text("water", "i chi gael dŵr")
      val bulletPointFive: Text = Text("an electric kettle", "bod gennych chi degell trydan")
      val bulletPointSix: Text = Text("an electricity supply", "bod gennych gyflenwad trydan")

      uiPage.components.head match {

        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 6

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems(2) shouldBe bulletPointThree
          b.listItems(3) shouldBe bulletPointFour
          b.listItems(four) shouldBe bulletPointFive
          b.listItems.last shouldBe bulletPointSix
        case _ => fail("Did not find bullet point list")
      }

    }

    "Process bullet point list in do you need to tell HMRC about extra income V6" in new Test {
      val ocelotPage = extraIncomeStanzaPages.head
      val uiPreProcessTransformations: Seq[(Seq[VisualStanza], Seq[VisualStanza]) => Seq[VisualStanza]] =
        Seq(BulletPointBuilder.groupBulletPointInstructions, RowAggregator.aggregateStanzas)

      val visualStanzas: Seq[VisualStanza] = ocelotPage.stanzas.collect{case s: VisualStanza => s}
      val uiPage = uiBuilder.buildPage(
        ocelotPage.url,
        uiPreProcessTransformations.foldLeft(visualStanzas){case (s, t) => t(s, Nil)}
      )(extraIncomeUrlMap)

      val leadingTextItems: Text = Text( "You've received income that you have not yet paid tax on from:",
                                         "Welsh: You've received income that you have not yet paid tax on from:")

      val bulletPointOne: Text = Text( "a business you own or control (such as a partnership or limited company)",
                                       "a business you own or control (such as a partnership or limited company)")

      val bulletPointTwo: Text = Text("a business a relative owns or controls", "a business a relative owns or controls")

      val bulletPointThree: Text = Text("your employer (for example for freelance services outside your normal contract hours)",
                                        "your employer (for example for freelance services outside your normal contract hours)")

      val bulletPointFour: Text = Text("the employer of your spouse or civil partner", "the employer of your spouse or civil partner")

      uiPage.components.head match {
        case b: BulletPointList =>

          b.text shouldBe leadingTextItems

          b.listItems.size shouldBe 4

          b.listItems.head shouldBe bulletPointOne
          b.listItems(1) shouldBe bulletPointTwo
          b.listItems(2) shouldBe bulletPointThree
          b.listItems.last shouldBe bulletPointFour

        case  _ => fail("First component should be a bullet point list")
      }
    }

  }

  trait InputTest {

    implicit val urlMap: Map[String, String] =
      Map(
        Process.StartStanzaId -> "/blah",
        "3" -> "dummy-path",
        "4" -> "dummy-path/input",
        "5" -> "dummy-path/blah",
        "6" -> "dummy-path/anotherinput",
        "34" -> "dummy-path/next"
      )
    val inputNext = Seq("4")
    val inputPhrase: Phrase = Phrase(Vector("Some Text", "Welsh, Some Text"))
    val helpPhrase: Phrase = Phrase(Vector("Help text", "Welsh, Help text"))

    val stanzas = Seq(
      KeyedStanza("start", PageStanza("/blah", Seq("1"), false)),
      KeyedStanza("1", Callout(Error, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("3"), false)),
      KeyedStanza("3", Callout(Section, Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("4"), false)),
      KeyedStanza("4", Instruction(Phrase(Vector("Some Text", "Welsh, Some Text")), Seq("end"), None, false))
    )
    val input1 = models.ocelot.stanzas.CurrencyInput(inputNext, inputPhrase, Some(helpPhrase), label ="input1", None, stack = false)
    val page = Page(Process.StartStanzaId, "/test-page", stanzas :+ KeyedStanza("5", input1), Seq.empty)

    val uiBuilder: UIBuilder = new UIBuilder()

    val four: Int = 4
  }
  "UIBuilder Input processing" must {

    "Ignore Error Callouts when there are no errors" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})(urlMap) match {
        case s: InputPage if s.input.errorMsgs.isEmpty => succeed
        case _: InputPage => fail("No error messages should be included on page")
        case x => fail(s"Should return InputPage: found $x")
      }
    }

    "Include Error messages when there are errors" in new InputTest {
      val formError = new FormError("test-page", List("error.required"))
      val formData = Some(FormData("test-page", Map(), List(formError)))

      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}, formData)(urlMap) match {
        case s: InputPage if s.input.errorMsgs.isEmpty => fail("No error messages found on page")
        case _: InputPage => succeed
        case x => fail(s"Should return InputPage: found $x")
      }
    }

    "Maintain order of components within an Input" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s}) match {
        case i: InputPage =>
          i.input.body(0) match {
            case _: H3 => succeed
            case _ => fail("Ordering of input body components not maintained")
          }
          i.input.body(1) match {
            case _: Paragraph => succeed
            case _ => fail("Ordering of input body components not maintained")
          }

        case x => fail(s"Should return InputPage: found $x")
      }

    }

    "Include a page hint appended to the input text" in new InputTest {
      uiBuilder.buildPage(page.url, page.stanzas.collect{case s: VisualStanza => s})(urlMap) match {
        case i: InputPage if i.input.hint == Some(Text("Help text", "Welsh, Help text")) => succeed
        case _: InputPage => fail("No hint found within Input")
        case x => fail(s"Should return InputPage: found $x")
      }
    }

  }

}
