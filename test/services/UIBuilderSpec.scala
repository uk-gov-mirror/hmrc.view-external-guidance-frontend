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
import models.ocelot.stanzas._
import models.ocelot._
import utils.StanzaHelper

class UIBuilderSpec extends BaseSpec with ProcessJson with StanzaHelper {

  trait Test extends ProcessJson {
    val flow = Map(
      "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      "1" -> InstructionStanza(0, Seq("2"), None, false),
      "2" -> QuestionStanza(1, Seq(2, 3), Seq("4", "5"), false),
      "4" -> InstructionStanza(0, Seq("end"), None, false),
      "5" -> InstructionStanza(0, Seq("end"), None, false),
      "end" -> EndStanza
    )
    val process = Process(metaSection, flow, Vector[Phrase](Phrase(Vector("Some Text","Welsh, Some Text")),
                                                            Phrase(Vector("Some Text1","Welsh, Some Text1")),
                                                            Phrase(Vector("Some Text2","Welsh, Some Text2")),
                                                            Phrase(Vector("Some Text3","Welsh, Some Text3"))), Vector[Link]())

    val singleOcelotPage = PageBuilder.buildPage("start", process).right.get
  }

  "UIBuilder" must {

    "convert and Ocelot page into a UI page with the same url" in new Test{

      UIBuilder.fromStanzaPage(singleOcelotPage) match {
        case Right(p) if p.urlPath == singleOcelotPage.url => succeed
        case Right(p) => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
        case Left(err) => fail(s"Failed with $err")
      }

    }

  }

}
