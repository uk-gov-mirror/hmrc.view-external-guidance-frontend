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

      val firstInstructionText: String = "The road is long and winding over there"
      val secondInstructionText: String = "The road is long and winding and here"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Not match instructions with multiple similar leading words but different spacing between the second and third words" in {

      val firstInstructionText: String = "The road   is long and winding over there"
      val secondInstructionText: String = "The road is long and winding and here"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
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

    "Not match instructions with three similar leading words in bold but different spacings between the first and second words" in {

      val firstInstructionText: String = "[bold:I have bought: apples]"
      val secondInstructionText: String = "[bold:I  have bought: oranges]"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
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

      val firstInstructionText: String = "Today [bold:is Monday] 1st"
      val secondInstructionText: String = "Today [bold:is Monday] 2nd"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe true
    }

    "Match instructions with multiple similar leading words with multiple sets of bold words" in {

      val firstInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday] 4th"
      val secondInstructionText: String = "Today is [bold:Monday and] tomorrow will [bold:be Tuesday] 7th"

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

    "Not match instructions with link in leading text but differing spaces between the second and third words" in {

      val firstInstructionText: String = "[link:The news  this: morning:https://mydomain/news/morning] Early riser fails to get up"
      val secondInstructionText: String = "[link:The news this: afternoon:https://mydomain/news/afternoon] Lunch goes missing"

      val firstInstructionPhrase: Phrase = createPhrase( firstInstructionText, "" )
      val secondInstructionPhrase: Phrase = createPhrase( secondInstructionText, "" )

      val i1: Instruction = new Instruction( firstInstructionPhrase, Nil, None, false )
      val i2: Instruction = new Instruction( secondInstructionPhrase, Nil, None, false )

      BulletPointListUtils.matchInstructions( i1, i2 ) mustBe false
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
