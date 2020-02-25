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

package utils

import models.ocelot.Phrase
import models.ocelot.stanzas.Instruction

import base.BaseSpec

class BulletPointListUtilsSpec extends BaseSpec {

  "BulletPointListUtils instruction bold text processing" must {

    "Manage  a blank text string" in {

      val text = ""

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe text
    }

    "Return text unchanged when no bold text present" in {

      val text: String = "Today the weather is fine"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe text
    }

    "Return bold text only when normal text is not defined" in {

      val text: String = "[bold:Important]"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "Important"
    }

    "Return both normal and bold text for combination of leading text followed by bold text" in {

      val text: String = "This is [bold:Important]"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "This is Important"
    }

    "Return both normal text and bold text for combination of leading bold text followed by normal text" in {

      val text: String = "[bold:Important] do not do this"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text) mustBe "Important do not do this"
    }

    "Return both normal and bold text for text with single embedded bold text" in {

      val text: String = "Hello from [bold:Team Ocelot] in Greenland"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "Hello from Team Ocelot in Greenland"
    }

    "Return both normal and bold text with normal text embedded in bold text" in {

      val text: String = "[bold:Greetings from] our home in lovely [bold:Nova Scotia]"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "Greetings from our home in lovely Nova Scotia"
    }

    "Return both normal and bold text from mixed text starting with normal text" in {

      val text: String = "Today is [bold:Wednesday 10th May] and tomorrow is [bold:Thursday 11th May]"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and bold text from mixed text staring with bold text" in {

      val text: String = "[bold:Here and now] we must all [bold:try] to be calm"

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( text ) mustBe "Here and now we must all try to be calm"
    }
  }

  "BulletPointListUtils instruction link text processing" must {

    "Manage a blank text string" in {

      val text = ""

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe text
    }

    "Return text unchanged when no link text present" in {

      val text: String = "Today the weather is fine"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe text
    }

    "Return link text only when normal text is not defined" in {

      val text: String = "[link:View options:https://mydomain/options]"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "View options"
    }

    "Return both normal and link text for combination of leading text followed by link text" in {

      val text: String = "View instructions for [link:mending a broken axle:http://mechanicsAreUs/axles]"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "View instructions for mending a broken axle"
    }

    "Return both normal text and link text for combination of leading link text followed by normal text" in {

      val text: String = "[link:Click here:https://my.com/details] for information"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "Click here for information"
    }

    "Return both normal and link text for text with single embedded link" in {

      val text: String = "For details [link:click here:https://info.co.uk/details] and follow the instructions shown"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "For details click here and follow the instructions shown"
    }

    "Return both normal and link text with normal text embedded in links" in {

      val text: String = "[link:Link 1 text:http://link1] and [link:link 2 text:https://link2]"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "Link 1 text and link 2 text"
    }

    "Return both normal and link text from mixed text starting with normal text" in {

      val text: String = "Today is [link:Wednesday 10th May:http://my.com/calendar] and tomorrow is [link:Thursday 11th May:http://my.com/calendar]"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and link text from mixed text staring with link" in {

      val text: String = "[link:Here and now:http://thisyear/today] we must all [link:try:https://explain] to be calm"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "Here and now we must all try to be calm"
    }

    "Return correct text with back to back links" in {

      val text: String = "This should [link:be interesting:https://my.com/interesting?part=2] [link:and informative:http://my.com/inform]"

      BulletPointListUtils.extractLinkPlaceHolderAnnotation( text ) mustBe "This should be interesting and informative"
    }

  }

  "Bullet point list instruction match testing" must {

    "Not match instructions with no similar text" in {

      val firstInstructionText: String = "Good Morning"
      val secondInstructionText: String = "Buen día"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Not match instructions with two similar leading words" in {

      val firstInstructionText: String = "Today is Wednesday"
      val secondInstructionText: String = "Today is Thursday"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Match instructions with three similar leading words" in {

      val firstInstructionText: String = "I have bought: apples"
      val secondInstructionText: String = "I have bought: oranges"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Match instructions with multiple similar leading words" in {

      val firstInstructionText: String = "The road is long and winding"
      val secondInstructionText: String = "The road is long and winding"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Not match instructions with two similar leading words in bold" in {

      val firstInstructionText: String = "[bold:Today is Monday]"
      val secondInstructionText: String = "[bold:Today is Thursday]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Match instructions with three similar leading words in bold" in {

      val firstInstructionText: String = "[bold:I have bought: apples]"
      val secondInstructionText: String = "[bold:I have bought: oranges]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Not match instructions with two similar leading words one normal text and one bold" in {

      val firstInstructionText: String = "Today [bold:is Monday]"
      val secondInstructionText: String = "Today [bold:is Thursday]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Match instructions with three similar leading words one normal text and two bold" in {

      val firstInstructionText: String = "Today [bold:is Monday]"
      val secondInstructionText: String = "Today [bold:is Monday]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Match instructions with multiple similar leading words with multiple sets of bold words" in {

      val firstInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday]"
      val secondInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Not match instructions with two similar leading words in links" in {

      val firstInstructionText: String = "[link:Today is Monday:http://mydomain/test]"
      val secondInstructionText: String = "[link:Today is Thursday:http://mydomain/test]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Match instructions with link in leading text" in {

      val firstInstructionText: String = "[link:The news this: morning:https://mydomain/news/morning] Early riser fails to get up"
      val secondInstructionText: String = "[link:The news this: afternoon:https://mydomain/news/afternoon] Lunch goes missing"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Match instructions with links in trailing text" in {

      val firstInstructionText: String = "Today I bought some [link:oranges:http://mydomain/fruits/oranges]"
      val secondInstructionText: String = "Today I bought some [link:apples:http://mydomain/fruits/apples]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Match instructions with complex leading and trailing text" in {

      val firstInstructionText: String = "Today [bold: I bought] some [link:fruits:http://mydomain/fruits] at [bold:Stafford Fruit Market] see [link:Staffordshire markets:https://mydomain/markets/staffordshire]"
      val secondInstructionText: String = "Today [bold: I bought] some [link:fruits:http://mydomain/fruits] at [bold:Shrewsbury Market] see [link:Shropshire markets:https://mydomain/markets/shropshire]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Does not match instruction text when both instructions have stack set to true" in {

      val firstInstructionText: String = "Today I bought some bananas"
      val secondInstructionText: String = "Today I bought some pears"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, true )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, true )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Does not match instruction text when first instruction has stack set to true and second instruction has stack set to false" in {

      val firstInstructionText: String = "Today I bought some bananas"
      val secondInstructionText: String = "Today I bought some pears"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, true )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }

    "Does not match instruction text when first instruction has stack set to false and second instruction has stack set to true" in {

      val firstInstructionText: String = "Today I bought some bananas"
      val secondInstructionText: String = "Today I bought some pears"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, true )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
    }
  }

  def createPhrase( englishText: String, welshText: String ) : Phrase = {

    Phrase( Vector( englishText, welshText ) )
  }

}
