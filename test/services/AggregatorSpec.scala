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

import models.ocelot.Phrase
import models.ocelot.stanzas._

import base.BaseSpec

class AggregatorSpec extends BaseSpec {

  private trait Test {

    val titlePhrase: Phrase = Phrase(Vector("Title", "Welsh, Title"))
    val instructionPhrase: Phrase = Phrase(Vector("Instruction", "Welsh, Instruction"))
    val instruction1Phrase: Phrase = Phrase(Vector("Instruction 1", "Welsh, Instruction 1"))
    val instruction2Phrase: Phrase = Phrase(Vector("Instruction 2", "Welsh, Instruction 2"))

    val r1c1: Phrase = Phrase(Vector("Row 1 - Cell 1", "Welsh, Row 1 - Cell 1"))
    val r1c2: Phrase = Phrase(Vector("Row 1 - Cell 2", "Welsh, Row 1 - Cell 2"))
    val r1c3: Phrase = Phrase(Vector("Row 1 - Cell 3", "Welsh, Row 1 - Cell 3"))

    val r1Cells: Seq[Phrase] = Seq(r1c1, r1c2, r1c3)

    val r2c1: Phrase = Phrase(Vector("Row 2 - Cell 1", "Welsh, Row 2 - Cell 1"))
    val r2c2: Phrase = Phrase(Vector("Row 2 - Cell 2", "Welsh, Row 2 - Cell 2"))

    val r2Cells: Seq[Phrase] = Seq(r2c1, r2c2)

    val r3c1: Phrase = Phrase(Vector("Row 3 - Cell 1", "Welsh, Row 3 - Cell 1"))
    val r3c2: Phrase = Phrase(Vector("Row 3 - Cell 2", "Welsh, Row 3 - Cell 2"))
    val r3c3: Phrase = Phrase(Vector("Row 3 - Cell 3", "Welsh, Row 3 - Cell 3"))
    val r3c4: Phrase = Phrase(Vector("Row 3 - Cell 4", "Welsh, Row 3 - Cell 4"))

    val r3Cells: Seq[Phrase] = Seq(r3c1, r3c2, r3c3, r3c4)

    val r4c1: Phrase = Phrase(Vector("Row 4 - Cell 1", "Welsh, Row 4 - Cell 1"))
    val r4c2: Phrase = Phrase(Vector("Row 4 - Cell 2", "Welsh, Row 4 - Cell 2"))

    val r4Cells: Seq[Phrase] = Seq(r4c1, r4c2)
    val r5Cells: Seq[Phrase] = Seq()

    val pageStanza: Stanza = PageStanza("/start", Seq(""), stack = false)

    val callout: Callout = TitleCallout(titlePhrase, Seq(""), stack = false)
    val instruction: Instruction = Instruction(instructionPhrase, Seq(""), None, stack = false)
    val instruction1: Instruction = Instruction(instruction1Phrase, Seq(""), None, stack = false)
    val instruction2: Instruction = Instruction(instruction2Phrase, Seq(""), None, stack = false)
    val instructionGroup: InstructionGroup = InstructionGroup(Seq(instruction, instruction1, instruction2))
  }

  private trait NumberedListTest extends Test {
    val num1Phrase = Phrase(Vector("Line1", "Welsh Line1"))
    val num2Phrase = Phrase(Vector("Line1", "Welsh Line1"))
    val num3Phrase = Phrase(Vector("Line1", "Welsh Line1"))
    val num4Phrase = Phrase(Vector("Line1", "Welsh Line1"))

    val num1ListCo = NumListCallout(num1Phrase, Seq(""), false)
    val num2ListCo = NumListCallout(num2Phrase, Seq(""), true)
    val num3ListCo = NumListCallout(num3Phrase, Seq(""), true)
    val num4ListCo = NumListCallout(num4Phrase, Seq(""), true)
    val num1CircListCo = NumCircListCallout(num1Phrase, Seq(""), false)
    val num2CircListCo = NumCircListCallout(num2Phrase, Seq(""), true)
    val num3CircListCo = NumCircListCallout(num3Phrase, Seq(""), true)
    val num4CircListCo = NumCircListCallout(num4Phrase, Seq(""), true)
  }

  "NumberedList aggregation" must {

    "add an isolated number list co with stack equals false into a row group of size 1" in new NumberedListTest {
      val stanzas: Seq[VisualStanza] =
        Seq(
          callout,
          num1ListCo,
          instruction)

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo))
    }

  "add an isolated number list co with stack equals true into a row group of size 1" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      num1ListCo,
      instruction
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo))
  }

  "create two number list groups for two contiguous rows with stack set to false" in new NumberedListTest {

    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      num1ListCo,
      num1ListCo,
      instruction
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo))
    aggregatedStanzas(2) shouldBe NumListGroup(Seq(num1ListCo))
  }

  "create a number list group with two entries for two contiguous number list cos with stack set to true" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      num2ListCo,
      num3ListCo,
      instruction
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num2ListCo, num3ListCo))
  }

  "create two number list groups for two contiguous number list cos with stack set to true and false respectively" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      instruction,
      num2ListCo,
      num1ListCo,
      instruction,
      callout
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num2ListCo))
    aggregatedStanzas(2) shouldBe NumListGroup(Seq(num1ListCo))
  }

  "create a number list group with two number list co with stack set to false and true respectively" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      instruction,
      num1ListCo,
      num2ListCo,
      instruction,
      callout
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo, num2ListCo))
  }

  "create a number list group with multiple number list cos" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      num1ListCo,
      num2ListCo,
      num3ListCo,
      num4ListCo,
      instruction
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo, num2ListCo, num3ListCo, num4ListCo))
  }

  "create two number list groups of size two from four contiguous elems where stack is false for the third elem" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      num2ListCo,
      num3ListCo,
      num1ListCo,
      num4ListCo
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(0) shouldBe NumListGroup(Seq(num2ListCo, num3ListCo))
    aggregatedStanzas(1) shouldBe NumListGroup(Seq(num1ListCo, num4ListCo))
  }

  "create two number list groups of size one for two non-contiguous elems in sequence of stanzas" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      instruction,
      num1ListCo,
      instruction1,
      num1ListCo,
      instruction2
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(2) shouldBe NumListGroup(Seq(num1ListCo))
    aggregatedStanzas(four) shouldBe NumListGroup(Seq(num1ListCo))
  }

  "create two number list groups with multiple elems from a complex sequence of stanzas" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      instruction,
      instruction1,
      num1ListCo,
      num2ListCo,
      num3ListCo,
      instructionGroup,
      num1ListCo,
      num2ListCo,
      instruction2
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(3) shouldBe NumListGroup(Seq(num1ListCo, num2ListCo, num3ListCo))
    aggregatedStanzas(five) shouldBe NumListGroup(Seq(num1ListCo, num2ListCo))
  }

  "create two number circle list groups with multiple elems from a complex sequence of stanzas" in new NumberedListTest {
    val stanzas: Seq[VisualStanza] = Seq(
      callout,
      instruction,
      instruction1,
      num1CircListCo,
      num2CircListCo,
      num3CircListCo,
      instructionGroup,
      num1CircListCo,
      num2CircListCo,
      instruction2
    )

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(3) shouldBe NumCircListGroup(Seq(num1CircListCo, num2CircListCo, num3CircListCo))
    aggregatedStanzas(five) shouldBe NumCircListGroup(Seq(num1CircListCo, num2CircListCo))
  }

}

  "Row aggregation" must {

    "add an isolated row with stack equals false into a row group of size 1" in new Test {

      val row: Row = Row(r1Cells, Seq(""))

      val stanzas: Seq[VisualStanza] = Seq(
        pageStanza,
        callout,
        row,
        instruction,
        EndStanza
      ).collect{case s: VisualStanza => s}

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(1) shouldBe RowGroup(Seq(row))
    }

  "add an isolated row with stack equals true into a row group of size 1" in new Test {

    val row: Row = Row(r1Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      row,
      instruction,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row))
  }

  "create two row groups for two contiguous rows with stack set to false" in new Test {

    val row1: Row = Row(r1Cells, Seq(""))
    val row2: Row = Row(r2Cells, Seq(""))

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      row1,
      row2,
      instruction,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row1))
    aggregatedStanzas(2) shouldBe RowGroup(Seq(row2))
  }

  "create a row group with two entries for two contiguous rows with stack set to true" in new Test {

    val row1: Row = Row(r1Cells, Seq(""), stack = true)
    val row2: Row = Row(r2Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      row1,
      row2,
      instruction,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row1, row2))
  }

  "create two row groups for two contiguous rows with stack set to true and false respectively" in new Test {

    val row1: Row = Row(r1Cells, Seq(""), stack = true)
    val row2: Row = Row(r2Cells, Seq(""))

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      instruction,
      row1,
      row2,
      instruction,
      callout,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row1))
    aggregatedStanzas(2) shouldBe RowGroup(Seq(row2))
  }

  "create a row group with two rows for two contiguous row stanzas with stack set to false and true respectively" in new Test {

    val row1: Row = Row(r1Cells, Seq(""))
    val row2: Row = Row(r2Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      instruction,
      row1,
      row2,
      instruction,
      callout,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row1, row2))
  }

  "create a row group with multiple rows" in new Test {

    val row1: Row = Row(r1Cells, Seq(""))
    val row2: Row = Row(r2Cells, Seq(""), stack = true)
    val row3: Row = Row(r3Cells, Seq(""), stack = true)
    val row4: Row = Row(r4Cells, Seq(""), stack = true)
    val row5: Row = Row(r5Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      row1,
      row2,
      row3,
      row4,
      row5,
      instruction,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(1) shouldBe RowGroup(Seq(row1, row2, row3, row4, row5))
  }

  "create two row groups of size two from four contiguous rows where stack is false for the third row" in new Test {

    val row1: Row = Row(r1Cells, Seq(""), stack = true)
    val row2: Row = Row(r2Cells, Seq(""), stack = true)
    val row3: Row = Row(r3Cells, Seq(""))
    val row4: Row = Row(r4Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      row1,
      row2,
      row3,
      row4,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(0) shouldBe RowGroup(Seq(row1, row2))
    aggregatedStanzas(1) shouldBe RowGroup(Seq(row3, row4))
  }

  "create two row groups of size one for two non-contiguous rows in sequence of stanzas" in new Test {

    val row1: Row = Row(r1Cells, Seq(""))
    val row2: Row = Row(r2Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      instruction,
      row1,
      instruction1,
      row2,
      instruction2,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
    aggregatedStanzas(2) shouldBe RowGroup(Seq(row1))
    aggregatedStanzas(four) shouldBe RowGroup(Seq(row2))
  }

  "create two row groups with multiple rows from a complex sequence of stanzas" in new Test {

    val row1: Row = Row(r1Cells, Seq(""))
    val row2: Row = Row(r2Cells, Seq(""), stack = true)
    val row3: Row = Row(r3Cells, Seq(""), stack = true)
    val row4: Row = Row(r4Cells, Seq(""))
    val row5: Row = Row(r5Cells, Seq(""), stack = true)

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      instruction,
      instruction1,
      row1,
      row2,
      row3,
      instructionGroup,
      row4,
      row5,
      instruction2,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas(3) shouldBe RowGroup(Seq(row1, row2, row3))
    aggregatedStanzas(five) shouldBe RowGroup(Seq(row4, row5))
  }

  "return input sequence of stanzas if sequence does not contain row stanzas" in new Test {

    val stanzas: Seq[VisualStanza] = Seq(
      pageStanza,
      callout,
      instruction,
      instructionGroup,
      EndStanza
    ).collect{case s: VisualStanza => s}

    val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

    aggregatedStanzas shouldBe stanzas
  }
}

  "Note aggregation" must {

    trait NoteTest extends Test {
      val phrase1 = Phrase(Vector("Note 1", "Welsh Note 1"))
      val phrase2 = Phrase(Vector("Note 2", "Welsh Note 2"))
      val phrase3 = Phrase(Vector("Note 3", "Welsh Note 3"))
      val phrase4 = Phrase(Vector("Note 4", "Welsh Note 4"))

      val callout1 = NoteCallout(phrase1, Seq(""), false)
      val callout2 = NoteCallout(phrase2, Seq(""), true)
      val callout3 = NoteCallout(phrase3, Seq(""), true)
      val callout4 = NoteCallout(phrase4, Seq(""), true)
    }

    "add an isolated note co with stack equals false into a note group of size 1" in new NoteTest {
      val stanzas: Seq[VisualStanza] =
        Seq(
          callout,
          callout1,
          instruction)

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1))
    }

    "add an isolated note co with stack equals true into a note group of size 1" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        callout1,
        instruction
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1))
    }

    "create two note groups for two contiguous rows with stack set to false" in new NoteTest {

      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        callout1,
        callout1,
        instruction
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1))
      aggregatedStanzas(2) shouldBe NoteGroup(Seq(callout1))
    }

    "create a note group with two entries for two contiguous note cos with stack set to true" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        callout2,
        callout3,
        instruction
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout2, callout3))
    }

    "create two note groups for two contiguous note cos with stack set to true and false respectively" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        instruction,
        callout2,
        callout1,
        instruction,
        callout
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout2))
      aggregatedStanzas(2) shouldBe NoteGroup(Seq(callout1))
    }

    "create a note group with two note co with stack set to false and true respectively" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        instruction,
        callout1,
        callout2,
        instruction,
        callout
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)

      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1, callout2))
    }

    "create a note group with multiple note cos" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        callout1,
        callout2,
        callout3,
        callout4,
        instruction
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1, callout2, callout3, callout4))
    }

    "create two note groups of size two from four contiguous elems where stack is false for the third elem" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout2,
        callout3,
        callout1,
        callout4
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(0) shouldBe NoteGroup(Seq(callout2, callout3))
      aggregatedStanzas(1) shouldBe NoteGroup(Seq(callout1, callout4))
    }

    "create two note groups of size one for two non-contiguous elems in sequence of stanzas" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        instruction,
        callout1,
        instruction1,
        callout1,
        instruction2
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(2) shouldBe NoteGroup(Seq(callout1))
      aggregatedStanzas(four) shouldBe NoteGroup(Seq(callout1))
    }

    "create two note groups with multiple elems from a complex sequence of stanzas" in new NoteTest {
      val stanzas: Seq[VisualStanza] = Seq(
        callout,
        instruction,
        instruction1,
        callout1,
        callout2,
        callout3,
        instructionGroup,
        callout1,
        callout2,
        instruction2
      )

      val aggregatedStanzas: Seq[Stanza] = Aggregator.aggregateStanzas(Nil)(stanzas)
      aggregatedStanzas(3) shouldBe NoteGroup(Seq(callout1, callout2, callout3))
      aggregatedStanzas(five) shouldBe NoteGroup(Seq(callout1, callout2))
    }

  }

}