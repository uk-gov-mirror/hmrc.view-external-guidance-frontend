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
  val sqpQpValue = Value( Scalar, "PageUrl", "/page/1" )
  val sqpFapValue = Value( Scalar, "PageUrl", "/page/2" )
  val sqpSapValue = Value( Scalar, "PageUrl", "/page/3" )

  // Question page
  val sqpQpValueStanza = ValueStanza( List( sqpQpValue ), Seq( "1" ), false )
  val sqpQpInstruction = Instruction( "SomeInstructionText", Seq( "2" ), None, false )
  val sqpQpCallout = Callout( SubTitle, "SomeCalloutText", Seq( "3" ), false )
  val sqpQpQuestion = Question( "SomeQuestion2", Seq( "three", "four" ), Seq( "4", "6" ), false )

  // First answer page
  val sqpFapValueStanza = ValueStanza( List( sqpFapValue ), Seq( "5" ), false )
  val sqpFapInstruction = Instruction( "SomeInstructionText2", Seq( "end" ), None, false )

  // Second answer page
  val sqpSapValueStanza = ValueStanza( List( sqpSapValue ), Seq( "7" ), false )
  val sqpSapInstruction = Instruction( "FIVE", Seq( "8" ), Some( 0 ), false  )
  val sqpSapCallout = Callout( Lede, "SIX", Seq( "end" ), false )

  def onePage:Map[String,Stanza] = {

    val value1 = Value(Scalar, "PageUrl", "/")
    Map(
      ("start" -> ValueStanza( List(value1), Seq("1"), false )),
      ("1" -> InstructionStanza(0,Seq("2"),None, false)),
      ("2" -> InstructionStanza(1,Seq("end"),None, false)),
      ("end" -> EndStanza)
    )
  }

  def twoPagesSeperatedByValueStanza:Map[String,Stanza] = {

    val value1 = Value(Scalar, "PageUrl", "/")
    val value2 = Value(Scalar, "PageUrl", "/a")
    Map(
      ("start" -> ValueStanza( List(value1), Seq("1"), false )),
      ("1" -> InstructionStanza(0,Seq("2"),None, false)),
      ("2" -> InstructionStanza(1,Seq("3"),None, false)),
      ("3" -> ValueStanza( List(value2), Seq("4"), false )),
      ("4" -> InstructionStanza(0,Seq("end"),None, false)),
      ("end" -> EndStanza)
    )
  }

  def simpleQuestionPage:Map[String,Stanza] = {

    // Define Map of Stanzas to be processed
    Map(
      "start" -> sqpQpValueStanza,
      "1" -> sqpQpInstruction,
      "2" -> sqpQpCallout,
      "3" -> sqpQpQuestion,
      "4" -> sqpFapValueStanza,
      "5" -> sqpFapInstruction,
      "6" -> sqpSapValueStanza,
      "7" -> sqpSapInstruction,
      "8" -> sqpSapCallout,
      "end" -> EndStanza
    )

  }

  def simpleQuestionPageAlt:Map[String,Stanza] = {

    // Define Map of Stanzas to be processed
    Map(
      "start" -> sqpQpValueStanza,
      "1" -> sqpQpInstruction,
      "2" -> sqpQpCallout,
      "3" -> sqpQpQuestion,
      "4" -> sqpFapValueStanza,
      "5" -> sqpFapInstruction,
      "6" -> sqpSapValueStanza,
      "7" -> sqpSapInstruction,
      "8" -> sqpSapCallout,
      "end" -> EndStanza
    )

  }

}
