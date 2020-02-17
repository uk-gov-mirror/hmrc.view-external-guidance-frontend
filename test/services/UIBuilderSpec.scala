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

    val stanzas = Map(
      "start" -> ValueStanza(List(Value(Scalar, "PageUrl", "/")), Seq("1"), false),
      "1" -> Instruction(Phrase(Vector("Some Text2","Welsh, Some Text2")), Seq("2"), None, false),
      "2" -> Callout(Title, Phrase(Vector("Some Text","Welsh, Some Text")), Seq("3"), false),
      "3" -> Callout(SubTitle, Phrase(Vector("Some Text1","Welsh, Some Text1")), Seq("4"), false),
      "4" -> Callout(Lede, Phrase(Vector("Some Text2","Welsh, Some Text2")), Seq("5"), false),
      "5" -> Instruction(Phrase(Vector("Some Text3","Welsh, Some Text3")), Seq("end"), None, false),
      "6" -> Instruction(Phrase(Vector("Some Text3","Welsh, Some Text3")), Seq("end"), None, false),
      "end" -> EndStanza
    )

    val page = Page("start", "/test-page", stanzas, Seq(""), Nil)
  }

  "UIBuilder" must {

    "convert and Ocelot page into a UI page with the same url" in new Test{

      UIBuilder.fromStanzaPage(page) match {
        case Right(p) if p.urlPath == page.url => succeed
        case Right(p) => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
        case Left(err) => fail(s"Failed with $err")
      }

    }

    // "convert Callout type Title to H1" in new Test{

    //   UIBuilder.fromStanzaPage(page) match {
    //     case Right(Page(_,_,s,_,_)) if s.get(1).isDefined =>

    //     case Left(err) => fail(s"Failed with $err")
    //   }

    // }
    // "convert Callout type SubTitle to H3" in new Test{

    //   UIBuilder.fromStanzaPage(page) match {
    //     case Right(p) if p.urlPath == singleOcelotPage.url => succeed
    //     case Right(p) => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
    //     case Left(err) => fail(s"Failed with $err")
    //   }

    // }
    // "convert Callout type Lede to lede Paragraph" in new Test{

    //   UIBuilder.fromStanzaPage(page) match {
    //     case Right(p) if p.urlPath == singleOcelotPage.url => succeed
    //     case Right(p) => fail(s"UI page urlPath set incorrectly to ${p.urlPath}")
    //     case Left(err) => fail(s"Failed with $err")
    //   }

    // }
  }

}
