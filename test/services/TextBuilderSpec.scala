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
import models.ocelot._
import models.ui.{Text, HyperLink, PageLink, TextItem}

class TextBuilderSpec extends BaseSpec {

  trait Test extends ProcessJson {

    val ltxt1 = Text("This is a ","Welsh, This is a ", true)
    val ltxt2 = Text(" followed by "," Welsh, followed by ")
    val ltxt3 = Text(" and nothing"," Welsh, and nothing")
    val link1Txt = Text("A link","Welsh, A link")
    val link2Txt = Text("Another Link","Welsh, Another Link")

    val link1 = HyperLink("https://www.bbc.co.uk", link1Txt, false)
    val link2 = HyperLink("https://www.gov.uk", link2Txt, false)
    val urlMap1: Map[String, String] = Map("3" -> "dummy-path", "5" -> "dummy-path/blah","34" -> "dummy-path/next")

    val txtWithLinks = Phrase(
      Vector("[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A link:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link:https://www.gov.uk] Welsh, and nothing")
    )

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]",
                                         "Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    implicit val stanzaIdToUrlMap: Map[String, String] = Map()
  }

  "TextBuilder placeholder parsing" must {
    "Convert a Text with link placeholders in lang strings to Seq[TextItem]" in new Test {

      val txtItems = TextBuilder.fromPhrase(txtWithLinks)

      txtItems(0) mustBe ltxt1
      txtItems(1) mustBe link1
      txtItems(2) mustBe ltxt2
      txtItems(3) mustBe link2
      txtItems(4) mustBe ltxt3
    }

    "leave syntactically incorrect link placeholders as text within a phrase" in new Test {
      val items: Seq[TextItem] = TextBuilder.fromPhrase(brokenLinkPhrase)(urlMap1)

      items.length mustBe 2

      items(0) mustBe Text("Hello ","Welsh, Hello ")
      items(1) mustBe Text("[link:Blah Blah:htts://www.bbc.co.uk]","[link:Blah Blah:htts://www.bbc.co.uk]")
    }

    "convert syntactically correct link placeholders into PageLink or HyperLink" in new Test {
      val linkPhrase = Phrase(Vector("Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]",
                                     "Welsh, Hello [link:Blah Blah:https://www.bbc.co.uk] [link:Blah Blah:5]"))

      val items: Seq[TextItem] = TextBuilder.fromPhrase(linkPhrase)(urlMap1)

      items.length mustBe 4

      items(0) mustBe Text("Hello ","Welsh, Hello ")
      items(1) mustBe HyperLink("https://www.bbc.co.uk",Text("Blah Blah","Blah Blah"))
      items(2) mustBe Text(" "," ")
      items(3) mustBe PageLink("dummy-path/blah",Text("Blah Blah","Blah Blah"))
    }

  }

}
