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

    "Return text unchanged when no bold text present" in {

      val englishText: String = "Today the weather is fine"
      val welshText: String = "Heddiw mae'r tywydd yn braf"

      val phrase: Phrase = createPhrase( englishText, welshText )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 0 ) mustBe englishText
      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 1 ) mustBe welshText
    }

    "Return bold text only when normal text is not defined" in {

      val englishText: String = "[bold:Important]"
      val welshText: String = "[bold:Pwysig]"

      val phrase: Phrase = createPhrase( englishText, welshText )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 0 ) mustBe "Important"
      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 1 ) mustBe "Pwysig"
    }

    "Return both normal and bold text for combination of leading text followed by bold text" in {

      val englishText: String = "This is [bold:Important]"
      val welshText: String = "Dyma [bold:Pwysig]"

      val phrase: Phrase = createPhrase( englishText, welshText )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 0 ) mustBe "This is Important"
      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 1 ) mustBe "Dyma Pwysig"
    }

    "Return both normal text and bold text for combination of leading bold text followed by normal text" in {

      val englishText: String = "[bold:Important] do not do this"
      val welshText: String = "[bold:Pwysig] peidiwch â gwneud hyn"

      val phrase: Phrase = createPhrase( englishText, welshText )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 0 ) mustBe "Important do not do this"
      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction, 1 ) mustBe "Pwysig peidiwch â gwneud hyn"
    }

    "Return both normal and bold text for text with single embedded bold text" in {

      val englishText: String = "Hello from [bold:Team SPECTRE] in Greenland"

      val phrase: Phrase = createPhrase( englishText, "" )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction ) mustBe "Hello from Team SPECTRE in Greenland"
    }

    "Return both normal and bold text with normal text embedded in bold text" in {

      val englishText: String = "[bold:Greetings from] our home in lovely [bold:Nova Scotia]"

      val phrase: Phrase = createPhrase( englishText, "" )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction ) mustBe "Greetings from our home in lovely Nova Scotia"
    }

    "Return both normal and bold text from mixed text starting with normal text" in {

      val englishText: String = "Today is [bold:Wednesday 10th May] and tomorrow is [bold:Thursday 11th May]"

      val phrase: Phrase = createPhrase( englishText, "" )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction ) mustBe "Today is Wednesday 10th May and tomorrow is Thursday 11th May"
    }

    "Return both normal and bold text from mixed text staring with bold text" in {

      val englishText: String = "[bold:Here and now] we must all [bold:try] to be calm"

      val phrase: Phrase = createPhrase( englishText, "" )

      val instruction: Instruction = Instruction( phrase, Nil, None, stack = false )

      BulletPointListUtils.extractBoldPlaceHolderAnnotation( instruction ) mustBe "Here and now we must all try to be calm"
    }
  }

  def createPhrase( englishText: String, welshText: String ) : Phrase = {

    Phrase( Vector( englishText, welshText ) )

  }

}
