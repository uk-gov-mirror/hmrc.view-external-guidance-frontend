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

package models.ocelot.stanzas

import org.scalatest.{MustMatchers, WordSpec}
import models.ocelot._

class InstructionSpec extends WordSpec with MustMatchers {

  trait Test {
    val text: Int = 10
    val next: Seq[String] = Seq("3")
    val linkId: Int = 0
    val stack: Boolean = false
    val linkDest = "4"
    val link = Link(linkId, linkDest, "", false)
    val pageLinkedStanzaId = "6"
    val phrase0 = Phrase(Vector(s"hello [link:Blah:${pageLinkedStanzaId}] ;lasdk ",s"Welsh, hello [link:Blah:${pageLinkedStanzaId}] ;lasdk "))
    val phrase1 = Phrase(Vector(s"hello [link:Blah:${linkDest}] ;lasdk [link:Blah:${pageLinkedStanzaId}]",
                                s"Welsh, hello [link:Blah:${linkDest}] ;lasdk [link:Blah:${pageLinkedStanzaId}]"))
    val simpleInstruction = InstructionStanza(text, next, None, false)
    val linkInstruction = InstructionStanza(text, next, Some(linkId), false)
  }

  "when constructed via apply() Instruction" must {

    "contain a list of linked stanza ids drawn from placeholders within the text" in new Test {
      val instruction = Instruction(simpleInstruction, phrase0, None)
      instruction.linkedStanzaIds.length mustBe 1

      instruction.linkedStanzaIds(0) mustBe pageLinkedStanzaId
    }

    "contain a list of linked stanza ids drawn from placeholders within the text and link" in new Test {
      val instruction = Instruction(linkInstruction, phrase0, Some(link))
      instruction.linkedStanzaIds.length mustBe 2

      instruction.linkedStanzaIds.contains(linkDest) mustBe true
      instruction.linkedStanzaIds.contains(pageLinkedStanzaId) mustBe true
    }

    "contain a list of unique linked stanza ids when used" in new Test {
      val instruction = Instruction(linkInstruction, phrase1, Some(link))
      instruction.linkedStanzaIds.length mustBe 2

      instruction.linkedStanzaIds.contains(linkDest) mustBe true
      instruction.linkedStanzaIds.contains(pageLinkedStanzaId) mustBe true
    }
  }

}
