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

package forms

import models.ui.SubmittedAnswer1
import play.api.data.Form

import base.BaseSpec

class SubmittedAnswerFormProviderSpec extends BaseSpec {

  private val answer: String = "0"

  val provider: SubmittedAnswerFormProvider = new SubmittedAnswerFormProvider()

  "Forms created by SubmittedAnswerFormProvider" should {

    "be able to be able to execute the unapply method held in mapping" in {

      val form: Form[SubmittedAnswer1] = provider(answer)
      val map: Map[String,String] = form.mapping.unbind(SubmittedAnswer1(""))
      val keySet = map.keySet

      keySet shouldBe Set(answer)
    }
  }

}
