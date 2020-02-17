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
import models.ui.Text

class UIBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  trait Test extends ProcessJson {

    val lang0 = Vector("Some Text","Welsh, Some Text")
    val lang1 = Vector("Some Text1","Welsh, Some Text1")
    val lang2 = Vector("Some Text2","Welsh, Some Text2")
    val lang3 = Vector("Some Text3","Welsh, Some Text3")
    val lang4 = Vector("Some Text4","Welsh, Some Text4")

    val stanzas = Seq(
      ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      Instruction(Phrase(lang2), Seq("2"), None, false),
      Callout(Title, Phrase(lang0), Seq("3"), false),
      Callout(SubTitle, Phrase(lang1), Seq("4"), false),
      Callout(Lede, Phrase(lang2), Seq("5"), false),
      Instruction(Phrase(lang3), Seq("end"), None, false),
      Instruction(Phrase(lang4), Seq("end"), Some(Link(7,"/somewhere","",false)), false),
      EndStanza
    )

    val page = Page("start", "/test-page", stanzas, Seq(""), Nil)
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

    "convert 2nd Callout type SubTitle to H3" in new Test{

      val uiPage = UIBuilder.fromStanzaPage(page)
      uiPage.components(2) mustBe models.ui.H3(Text(lang1))
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

  }

}
