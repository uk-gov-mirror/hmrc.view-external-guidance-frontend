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

package models.ui

import base.BaseSpec
import models.ocelot._
import models.ocelot.stanzas._

class StackingSpec extends BaseSpec {

  trait Test {
    def stackedCo(typ: CalloutType, stack: Boolean) = Callout(typ, Phrase(), Seq.empty, stack)
    def stackedRowGroup(stack: Boolean): RowGroup = RowGroup(Seq.empty, Seq.empty, stack)
  }

  "Stacking" should {
    "Group stacked stanzas together" in new Test {
      val stanzas = Seq(stackedCo(Title, false), stackedCo(Title, false), stackedCo(Title, true), stackedCo(Title, false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 3
      stacked(0) shouldBe stackedCo(Title, false)
      stacked(1) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedCo(Title, true)))
      stacked(2) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedRowGroup(true)))
    }

    "Group stacked stanzas including initial isolated unstackable stanza marked stack" in new Test {
      val stanzas = Seq(stackedCo(Title, true), stackedCo(Title, false), stackedCo(Title, true), stackedCo(Title, false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 3
      stacked(0) shouldBe stackedCo(Title, true)
      stacked(1) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedCo(Title, true)))
      stacked(2) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedRowGroup(true)))
    }

    "Group stacked stanzas including embedded isolated unstackable stanza marked stack" in new Test {
      val stanzas = Seq(stackedCo(Title, true), stackedCo(Title, false), stackedCo(Error, false), stackedCo(Title, false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)

      stacked.length shouldBe 4
      stacked(0) shouldBe stackedCo(Title, true)
      stacked(1) shouldBe stackedCo(Title, false)
      stacked(2) shouldBe stackedCo(Error, false)
      stacked(3) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedRowGroup(true)))
    }


    "Group stacked stanzas in alternate group" in new Test {
      val stanzas = Seq(stackedCo(Title, true), stackedCo(Title, true), stackedCo(Error, false), stackedCo(Title, false), stackedRowGroup(true))

      val stacked = stackStanzas(Nil)(stanzas)
      stacked.length shouldBe 3
      stacked(0) shouldBe StackedGroup(Seq(stackedCo(Title, true), stackedCo(Title, true)))
      stacked(1) shouldBe stackedCo(Error, false)
      stacked(2) shouldBe StackedGroup(Seq(stackedCo(Title, false), stackedRowGroup(true)))
    }

    "Group stacked stanzas in alternate group 2" in new Test {
      val stanzas = Seq(stackedCo(Title, true), stackedCo(Title, true), stackedCo(Error, true), stackedCo(Title, true), stackedRowGroup(false))

      val stacked = stackStanzas(Nil)(stanzas)
      stacked.length shouldBe 2
      stacked(0) shouldBe StackedGroup(Seq(stackedCo(Title, true), stackedCo(Title, true), stackedCo(Error, true), stackedCo(Title, true)))
      stacked(1) shouldBe stackedRowGroup(false)
    }

  }
}
