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
import models.ocelot.stanzas.Instruction
import models.ui.{HyperLink, PageLink, Text, TextItem}

import scala.util.matching.Regex.Match

class TextBuilderSpec extends BaseSpec {

  trait Test extends ProcessJson {

    val ltxt1 = Words("This is a ","Welsh, This is a ", true)
    val ltxt2 = Words(" followed by "," Welsh, followed by ")
    val ltxt3 = Words(" and nothing"," Welsh, and nothing")
    val link1Txt = Words("A link","Welsh, A link")
    val link2Txt = Words("Another Link","Welsh, Another Link")

    val link1 = Link("https://www.bbc.co.uk", link1Txt, false)
    val link2 = Link("https://www.gov.uk", link2Txt, false)
    val urlMap1: Map[String, String] = Map("3" -> "dummy-path", "5" -> "dummy-path/blah","34" -> "dummy-path/next")

    val txtWithLinks = Phrase(
      Vector("[bold:This is a ][link:A link:https://www.bbc.co.uk] followed by [link:Another Link:https://www.gov.uk] and nothing",
      "[bold:Welsh, This is a ][link:Welsh, A link:https://www.bbc.co.uk] Welsh, followed by [link:Welsh, Another Link:https://www.gov.uk] Welsh, and nothing")
    )

    val brokenLinkPhrase = Phrase(Vector("Hello [link:Blah Blah:htts://www.bbc.co.uk]",
                                         "Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]"))
    implicit val stanzaIdToUrlMap: Map[String, String] = Map()

    val answerWithNoHint = Phrase(Vector("Yes", "Welsh, Yes"))
    val answerWithHint = Phrase(Vector("Yes[hint:You agreee with the assertion]", """Welsh, Yes[hint:You DON'T agree with the assertion]"""))
    val answer = Text("Yes","Welsh, Yes")
    val hint = Text("You agreee with the assertion","""You DON'T agree with the assertion""")
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

      items.length mustBe 1

      items(0) mustBe Text("Hello [link:Blah Blah:htts://www.bbc.co.uk]","Welsh, Hello [link:Blah Blah:htts://www.bbc.co.uk]")
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

  "TextBuilder bold text annotation removal processing" must {

    "Manage  a blank text string" in {

      val text = ""

      TextBuilder.fragmentsToDisplay( text ) mustBe text
    }

    "Return text unchanged when no bold text present" in {

      val text: String = "Today the weather is fine"

      TextBuilder.fragmentsToDisplay( text ) mustBe text
    }

    "Return bold text only when normal text is not defined" in {

      val text: String = "[bold:Important]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Important"
    }

    "Return both normal and bold text for combination of leading text followed by bold text" in {

      val text: String = "This is [bold:Important]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "This is Important"
    }

    "Return both normal text and bold text for combination of leading bold text followed by normal text" in {

      val text: String = "[bold:Important] do not do this"

      TextBuilder.fragmentsToDisplay( text) mustBe "Important do not do this"
    }

    "Return both normal and bold text for text with single embedded bold text" in {

      val text: String = "Hello from [bold:Team Ocelot] in Greenland"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Hello from Team Ocelot in Greenland"
    }

    "Return both normal and bold text with normal text embedded in bold text" in {

      val text: String = "[bold:Greetings from] our home in lovely [bold:Nova Scotia]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Greetings from our home in lovely Nova Scotia"
    }

    "Return both normal and bold text from mixed text starting with normal text" in {

      val text: String = "Today is [bold:Wednesday 10th May] and tomorrow is [bold:Thursday 11th May]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and bold text from mixed text staring with bold text" in {

      val text: String = "[bold:Here and now] we must all [bold:try] to be calm"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Here and now we must all try to be calm"
    }
  }

  "TextBuilder answer processing" must {
    "return display answer text only when there is no hint" in new Test {
      val(displayText, hintText) = TextBuilder.answerTextWithOptionalHint(answerWithNoHint)
      displayText mustBe Text(answerWithNoHint.langs)
      hintText mustBe None
    }

    "return display answer text with hint" in new Test {
      val(displayText, hintText) = TextBuilder.answerTextWithOptionalHint(answerWithHint)
      displayText mustBe answer
      hintText mustBe Some(hint)
    }
  }

  "TextBuilder link text annotation removal processing" must {

    "Manage a blank text string" in {

      val text = ""

      TextBuilder.fragmentsToDisplay( text ) mustBe text
    }

    "Return text unchanged when no link text present" in {

      val text: String = "Today the weather is fine"

      TextBuilder.fragmentsToDisplay( text ) mustBe text
    }

    "Return link text only when normal text is not defined" in {

      val text: String = "[link:View options:https://mydomain/options]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "View options"
    }

    "Return both normal and link text for combination of leading text followed by link text" in {

      val text: String = "View instructions for [link:mending a broken axle:http://mechanicsAreUs/axles]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "View instructions for mending a broken axle"
    }

    "Return both normal text and link text for combination of leading link text followed by normal text" in {

      val text: String = "[link:Click here:https://my.com/details] for information"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Click here for information"
    }

    "Return both normal and link text for text with single embedded link" in {

      val text: String = "For details [link:click here:https://info.co.uk/details] and follow the instructions shown"

      TextBuilder.fragmentsToDisplay( text ) mustBe "For details click here and follow the instructions shown"
    }

    "Return both normal and link text with normal text embedded in links" in {

      val text: String = "[link:Link 1 text:http://link1] and [link:link 2 text:https://link2]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Link 1 text and link 2 text"
    }

    "Return both normal and link text from mixed text starting with normal text" in {

      val text: String = "Today is [link:Wednesday 10th May:http://my.com/calendar] and tomorrow is [link:Thursday 11th May:http://my.com/calendar]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and link text from mixed text staring with link" in {

      val text: String = "[link:Here and now:http://thisyear/today] we must all [link:try:https://explain] to be calm"

      TextBuilder.fragmentsToDisplay( text ) mustBe "Here and now we must all try to be calm"
    }

    "Return correct text with back to back links" in {

      val text: String = "This should [link:be interesting:https://my.com/interesting?part=2] [link:and informative:http://my.com/inform]"

      TextBuilder.fragmentsToDisplay( text ) mustBe "This should be interesting and informative"
    }

  }
}