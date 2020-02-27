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

import models.ocelot.stanzas.Instruction
import services.TextBuilder


object BulletPointListUtils {

  def matchInstructions( i1: Instruction, i2: Instruction ) : Boolean = {

    // Apply matching logic to English text as this is how Ocelot works currently
    if( !i1.stack  && !i2.stack ) {

      // Apply matching logic to English text as this is how Ocelot works currently
      val i1Text: String = TextBuilder.extractBoldAndLinkTextAnnotation(i1.text.langs(0))

      val i2Text: String = TextBuilder.extractBoldAndLinkTextAnnotation(i2.text.langs(0))

      val i1TextList: List[String] = i1Text.trim().split( " +" ).toList
      val i2TextList: List[String] = i2Text.trim().split( " +" ).toList

      val matchedTextItems: List[String] = (i1TextList zip i2TextList).takeWhile(t => t._1 == t._2).map(_._1)

      if (matchedTextItems.size >= matchLimit) true else false
    } else {
      false
    }
  }

  val matchLimit: Int = 3
}
