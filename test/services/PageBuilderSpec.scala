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

import play.api.libs.json._
import base.{BaseSpec, ProcessJson}
import models.ocelot._
import models.ocelot.stanzas._
import utils.StanzaHelper

class PageBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  val meta: Meta = Json.parse( prototypeMetaSection ).as[Meta]

  case object DummyStanza extends Stanza

  "PageBuilder error handling" must {

    val flow = Map(
      ("start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false)),
      ("1" -> InstructionStanza(0,Seq("2"), None, false)),
      ("2" -> DummyStanza)
    )

    "detect UnknownStanza error" in {

      val process = Process(metaSection, flow, Vector[Phrase](), Vector[Link]())

      PageBuilder.buildPage("start", process) match {
        case Left(UnknownStanza(DummyStanza)) => succeed
        case _ => fail("Unknown stanza not detected")
      }

    }

    "detect NoSuchPage error" in {
      val process = Process(metaSection, flow, Vector[Phrase](), Vector[Link]())

      PageBuilder.buildPage("4", process) match {
        case Left(NoSuchPage("4")) => succeed
        case _ => fail("Unknown stanza not detected")
      }
    }

    "detect MissingPageUrlValueStanza error" in {
      val flow = Map(
        ("start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false)),
        ("1" -> InstructionStanza(0,Seq("2"), None, false)),
        ("2" -> QuestionStanza(1, Seq(2,3), Seq("4","5"), false)),
        ("4" -> InstructionStanza(0,Seq("end"), None, false)),
        ("5" -> InstructionStanza(0,Seq("end"), None, false)),
        ("end" -> EndStanza)
      )
      val process = Process(metaSection, flow, Vector[Phrase](), Vector[Link]())

      PageBuilder.pages(process, "start") match {
        case Left(MissingPageUrlValueStanza("4")) => succeed
        case _ => fail(s"Missing ValueStanza containing PageUrl value not detected")
      }
    }
  }

  "PageBuilder" must {

    "be not buildable from non-existent key" in {


      val process:Process = prototypeJson.as[Process]

      PageBuilder.buildPage("unknown", process) match {
        case Right(_) => fail( "Invalid key should not return a page")
        case Left(err) => succeed
      }
    }

    "Sequence of connected pages" must {

      "not be extractable from a Process using an invalid start key" in {

        val process:Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "unknown") match {
          case Right(_) => fail("""Should fail with NoSuchPage("unknown")""")
          case Left(err) if err == NoSuchPage("unknown") => succeed
          case Left(wrongErr) => fail("""Should fail with NoSuchPage("unknown")""")
        }
      }

      "be extractable from a Process using key 'start''" in {

        val process:Process = prototypeJson.as[Process]

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 27

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "consist of one page when only page exists" in {
        val process:Process = validOnePageJson.as[Process]

        PageBuilder.pages(process, "1") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }

      }

      "confirm one page elements" in {

        val process:Process = Process( meta, onePage, Vector(), Vector())

        PageBuilder.pages(process, "start") match {
          case Right(pages) =>
            pages mustNot be (Nil)

            pages.length mustBe 1

          case Left(err) => fail(s"FlowError $err")
        }
      }

    }

  }

}
