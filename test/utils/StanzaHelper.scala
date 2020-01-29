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

import models.ocelot.stanzas._

trait StanzaHelper {

  // Define simple constants for use when creating stanzas
  val two: Int = 2
  val three: Int = 3
  val four: Int = 4
  val five: Int = 5
  val six: Int = 6

  // Define stanzas used in simple question page test
  val sqpQpValue = Value( Scalar, "pageUrl", "/page/1" )
  val sqpFapValue = Value( Scalar, "pageUrl", "/page/2" )
  val sqpSapValue = Value( Scalar, "pageUrl", "/page/3" )

  // Question page
  val sqpQpValueStanza = ValueStanza( List( sqpQpValue ), Seq( "1" ), false )
  val sqpQpInstructionStanza = InstructionStanza( 0, Seq( "2" ), None, false )
  val sqpQpCalloutStanza = CalloutStanza( SubTitle, 1, Seq( "3" ), false )
  val sqpQpQuestionStanza = QuestionStanza( 2, Seq( three, four ), Seq( "4", "6" ), false )

  // First answer page
  val sqpFapValueStanza = ValueStanza( List( sqpFapValue ), Seq( "5" ), false )
  val sqpFapInstructionStanza = InstructionStanza( 2, Seq( "end" ), None, false )

  // Second answer page
  val sqpSapValueStanza = ValueStanza( List( sqpSapValue ), Seq( "7" ), false )
  val sqpSapInstructionStanza = InstructionStanza( five, Seq( "8" ), Some( 0 ), false  )
  val sqpSapCalloutStanza = CalloutStanza( Lede, six, Seq( "end" ), false )

  def onePage:Map[String,Stanza] = {

    val value1 = Value(Scalar, "PageUrl", "/")
    Map(
      ("start" -> ValueStanza( List(value1), Seq("1"), false )),
      ("1" -> InstructionStanza(0,Seq("2"),None, false)),
      ("2" -> InstructionStanza(1,Seq("end"),None, false)),
      ("end" -> EndStanza)
    )
  }

  def simpleQuestionPage:Map[String,Stanza] = {

    // Define Map of Stanzas to be processed
    Map(
      "start" -> sqpQpValueStanza,
      "1" -> sqpQpInstructionStanza,
      "2" -> sqpQpCalloutStanza,
      "3" -> sqpQpQuestionStanza,
      "4" -> sqpFapValueStanza,
      "5" -> sqpFapInstructionStanza,
      "6" -> sqpSapValueStanza,
      "7" -> sqpSapInstructionStanza,
      "8" -> sqpSapCalloutStanza,
      "end" -> EndStanza
    )

  }
}
