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

package mocks

import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory

import models.ocelot.stanzas.VisualStanza
import models.ui.FormData
import services.UIBuilder

trait MockUIBuilder extends MockFactory {

  val mockUIBuilder: UIBuilder = mock[UIBuilder]

  object MockUIBuilder {

    def fromStanzas(url: String, stanzas: Seq[VisualStanza], formData: Option[FormData]): CallHandler[models.ui.Page] = {

      (mockUIBuilder
        .fromStanzas(_: String, _: Seq[VisualStanza], _: Option[FormData])(_: Map[String, String]))
        .expects(url, stanzas, *, *)
    }

  }

}
