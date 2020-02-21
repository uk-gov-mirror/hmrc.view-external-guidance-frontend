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

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

import models.ocelot.stanzas.Instruction

object BulletPointListUtils {

  def matchInstructions( i1: Instruction, i2: Instruction ) : Boolean = {

    true
  }

  /**
   * Method constructs text content of instruction without bold place holder annotation.
   *
   * @param instruction
   * @param langIndex
   * @return
   */
  def extractBoldPlaceHolderAnnotation( instruction: Instruction, langIndex: Int = 0 ) : String = {

    // Match text for bold text
    val boldTextMatches: List[Match] = boldTextRegex.findAllMatchIn( instruction.text.langs( langIndex ) ).toList

    // Find additional text components
    val texts: List[String] = boldTextRegex.split( instruction.text.langs( langIndex ) ).toList

    if( texts.size == 1 && boldTextMatches.size == 0 ) {
      instruction.text.langs( langIndex )
    } else if( texts.size == 0 && boldTextMatches.size == 1 ) {
      boldTextMatches.head.group(1)
    } else {
      val boldTexts: List[String] = boldTextMatches.map( _.group(1) )

      val startsWithTexts: Boolean = instruction.text.langs( langIndex ).startsWith( texts.head )

      val mergedTexts: List[String] = mergeTexts( texts, boldTexts, startsWithTexts )

      mergedTexts.mkString
    }
  }

  private def mergeTexts( texts: List[String], matchedTexts: List[String], startWithText: Boolean ) : List[String] = {

    if (startWithText) {
      texts.zipAll(matchedTexts, "", "") flatMap { case (a, b) => Seq(a, b) }
    } else {
      matchedTexts.zipAll(texts, "", "") flatMap { case (a, b) => Seq(a, b) }
    }

  }

  val boldTextRegex: Regex = """\[bold:([^\]]*)\]""".r
}
