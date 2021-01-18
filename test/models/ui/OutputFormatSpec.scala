/*
 * Copyright 2021 HM Revenue & Customs
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

class OutputFormatSpec extends BaseSpec {

  "Output formats" should {
    "identify as numeric for numeric formats" in {
      Currency.isNumeric shouldBe true
      CurrencyPoundsOnly.isNumeric shouldBe true
      Number.isNumeric shouldBe true
    }

    "identify as not numeric for non-numeric formats" in {
      Txt.isNumeric shouldBe false
      DateStandard.isNumeric shouldBe false
    }
  }
}
