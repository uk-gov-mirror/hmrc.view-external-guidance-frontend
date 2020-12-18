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

package models

import models.ui.{Page => UiPage, FormPage, Text}
import models.ocelot.{Page, LabelCache, Labels}
import models.ocelot.stanzas.DataInput

case class FormEvaluationContext(uiPage: FormPage,
                                 page: Page,
                                 dataInput: Option[DataInput],
                                 sessionId: String,
                                 stanzaIdToUrlMap: Map[String, String],
                                 processStartUrl: Option[String],
                                 processTitle: Text,
                                 processId: String,
                                 processCode: String,
                                 labels: Labels = LabelCache(),
                                 backLink: Option[String] = None,
                                 answer: Option[String] = None)

case class PageEvaluationContext(uiPage: UiPage,
                                 page: Page,
                                 dataInput: Option[DataInput],
                                 sessionId: String,
                                 stanzaIdToUrlMap: Map[String, String],
                                 processStartUrl: Option[String],
                                 processTitle: Text,
                                 processId: String,
                                 processCode: String,
                                 labels: Labels = LabelCache(),
                                 backLink: Option[String] = None,
                                 answer: Option[String] = None)

object FormEvaluationContext {
  def apply(page: FormPage, ctx: PageEvaluationContext): FormEvaluationContext =
    FormEvaluationContext(
      page,
      ctx.page,
      ctx.dataInput,
      ctx.sessionId,
      ctx.stanzaIdToUrlMap,
      ctx.processStartUrl,
      ctx.processTitle,
      ctx.processId,
      ctx.processCode,
      ctx.labels,
      ctx.backLink,
      ctx.answer
    )
}

case class PageContext(page: UiPage,
                       sessionId: String,
                       processStartUrl: Option[String],
                       processTitle: Text,
                       processId: String,
                       processCode: String,
                       labels: Labels = LabelCache(),
                       backLink: Option[String] = None,
                       answer: Option[String] = None)

object PageContext {
  def apply(pec: FormEvaluationContext): PageContext =
    PageContext(
      pec.uiPage,
      pec.sessionId,
      pec.processStartUrl,
      pec.processTitle,
      pec.processId,
      pec.processCode,
      pec.labels,
      pec.backLink,
      pec.answer
    )

  def apply(pec: PageEvaluationContext): PageContext =
    PageContext(
      pec.uiPage,
      pec.sessionId,
      pec.processStartUrl,
      pec.processTitle,
      pec.processId,
      pec.processCode,
      pec.labels,
      pec.backLink,
      pec.answer
    )
}
