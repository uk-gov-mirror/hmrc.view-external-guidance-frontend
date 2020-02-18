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
import utils.StanzaHelper
import models.ui.{Text, HyperLink}

class UIBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  trait Test extends ProcessJson {

    val lang0 = Vector("Some Text","Welsh, Some Text")
    val lang1 = Vector("Some Text1","Welsh, Some Text1")
    val lang2 = Vector("Some Text2","Welsh, Some Text2")
    val lang3 = Vector("Some Text3","Welsh, Some Text3")
    val lang4 = Vector("Some Text4","Welsh, Some Text4")

    val ltxt1 = Text("This is a ","Welsh, This is a ")
    val ltxt2 = Text(" followed by "," followed by ")
    val ltxt3 = Text(" and nothing"," and nothing")
    val link1Txt = Text("A link","A link")
    val link2Txt = Text("Another Link","Another Link")

    val link1 = HyperLink("https://www.bbc.co.uk", link1Txt, false)
    val link2 = HyperLink("https://www.gov.uk", link2Txt, false)
    val txtWithLinks = Phrase(
      Vector("This is a [link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
      "Welsh, This is a [link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing")
    )

    val linkInstructionStanza = Instruction(Phrase(lang4), Seq("end"), Some(Link(7,"/somewhere","",false)), false)
    val embeddedLinkInstructionStanza = Instruction(txtWithLinks, Seq("end"), None, false)
    val initialStanza = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Callout(Title, Phrase(lang0), Seq("3"), false),
      Callout(SubTitle, Phrase(lang1), Seq("4"), false),
      Callout(Lede, Phrase(lang2), Seq("5"), false),
      Instruction(Phrase(lang3), Seq("end"), None, false)
    )
    val stanzas = initialStanza ++ Seq(linkInstructionStanza, EndStanza)
    val stanzasWithEnbeddedLinks = initialStanza ++ Seq(embeddedLinkInstructionStanza, EndStanza)
    val page = Page("start", "/test-page", stanzas, Seq(""), Nil)

    val textItems = Seq(ltxt1, link1, ltxt2, link2, ltxt3)

    val pageWithEmbeddLinks = page.copy(stanzas = stanzasWithEnbeddedLinks)
  }

  "UIBuilder placeholder parsing" must {
    "Convert a Text with link placeholders in lang strings to Seq[TextItem]" in new Test {

      val txtItems = UIBuilder.fromText(txtWithLinks)

      txtItems(0) mustBe ltxt1
      txtItems(1) mustBe link1
      txtItems(2) mustBe ltxt2
      txtItems(3) mustBe link2
      txtItems(4) mustBe ltxt3
    }
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
      uiPage.components(5) mustBe models.ui.Paragraph(Seq(models.ui.HyperLink("/somewhere", Text(lang4), false)), false)
    }

    "convert instruction within embedded links to Paragraph with a sequence of Text and HyperLink text items" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(pageWithEmbeddLinks)
      uiPage.components(5) mustBe models.ui.Paragraph(textItems, false)
    }

  }


}
