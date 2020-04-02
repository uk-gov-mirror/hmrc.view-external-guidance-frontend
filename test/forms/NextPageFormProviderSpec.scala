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

import org.scalatest.{Matchers, WordSpec}
import play.api.data.Form

import models.ui.NextPageUrl

class NextPageFormProviderSpec extends WordSpec with Matchers {

  private val url: String = "/somewhere"

  val provider: NextPageFormProvider = new NextPageFormProvider()

  "Forms created by NextPageFormProvider" should {

    "Be able to execute the unapply method held in mapping" in {

      val form: Form[NextPageUrl] = provider(url)
      val map: Map[String,String] = form.mapping.unbind(NextPageUrl(""))
      val keySet = map.keySet

      keySet shouldBe Set( url )
    }

  }

}
