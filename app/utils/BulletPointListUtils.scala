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

import scala.annotation.tailrec


object BulletPointListUtils {

  def matchInstructions( i1: Instruction, i2: Instruction ) : Boolean = {

    // Apply matching logic to English text as this is how Ocelot works currently
    if( !i1.stack  && !i2.stack ) {

      // Apply matching logic to English text as this is how Ocelot works currently
      val i1Text: String = extractLinkPlaceHolderAnnotation(extractBoldPlaceHolderAnnotation(i1.text.langs(0)))

      val i2Text: String = extractLinkPlaceHolderAnnotation(extractBoldPlaceHolderAnnotation(i2.text.langs(0)))

      val i1TextList: List[String] = i1Text.split( ' ' ).toList.map( _.trim )
      val i2TextList: List[String] = i2Text.split( ' ' ).toList.map( _.trim )

      val matchedTextItems: Seq[String] = listMatch( i1TextList, i2TextList, Nil)

      if (matchedTextItems.size >= matchLimit) true else false
    } else {
      false
    }
  }

  def extractBoldPlaceHolderAnnotation( txt: String ) : String = {

    extractPlaceHolderAnnotation( boldTextRegex, txt )
  }

  def extractLinkPlaceHolderAnnotation( txt: String ) : String = {

    extractPlaceHolderAnnotation( linkRegex, txt )
  }

  def extractPlaceHolderAnnotation( matcher: Regex, txt: String ) : String = {

    // Find matches with regex
    val matches: List[Match] = matcher.findAllMatchIn(txt).toList

    // Find additional text components
    val texts: List[String] = matcher.split(txt).toList

    if (texts.size == 1 && matches.size == 0) {
      texts.head
    } else if (texts.size == 0 && matches.size == 1) {
      matches.head.group(1)
    } else {
      val matchTexts: List[String] = matches.map(_.group(1))

      val mergedTexts: List[String] = texts.zipAll(matchTexts, "", "") flatMap { case (a, b) => Seq(a, b) }

      mergedTexts.mkString
    }
  }

  @tailrec
  def listMatch( list1: List[String], list2: List[String], acc: Seq[String] ) : Seq[String] = {

    ( list1, list2 ) match {
      case (Nil,Nil) => acc
      case (_, Nil) => acc
      case(Nil, _) => acc
      case ( item1  :: list1xs, item2 :: list2xs ) if( item1 == item2 ) => listMatch( list1xs, list2xs, (acc :+ item1 ))
      case _ => acc
    }
  }

  val boldTextRegex: Regex = """\[bold:([^\]]*)\]""".r
  val linkRegex: Regex = """\[link:(.*?):(http[s]?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r

  val matchLimit: Int = 3
}
