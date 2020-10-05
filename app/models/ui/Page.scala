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

import models.ocelot.{Labels, LabelCache, Page => OcelotPage}

sealed trait Page {
  val heading: Text
  val urlPath: String
  val components: Seq[UIComponent]
  val relativePath = urlPath.dropWhile(_ == '/')
}

object Page {

  def apply(urlPath: String, components: Seq[UIComponent]): Page =
    components match {
      case (question: Question) :: _ => QuestionPage(urlPath, question)
      case _ => StandardPage(urlPath, components)
    }
}

case class StandardPage(val urlPath: String, val components: Seq[UIComponent]) extends Page {

  val heading: Text = components
    .find {
      case _: H1 => true
      case _ => false
    }
    .fold(Text())(_.text)
}

case class QuestionPage(val urlPath: String, question: Question) extends Page {
  val heading: Text = question.text
  val components: Seq[UIComponent] = Seq(question)
}

case class PageEvaluationContext(page: OcelotPage,
                                 stanzaIdToUrlMap: Map[String, String],
                                 processStartUrl: Option[String],
                                 processTitle: Text,
                                 processId: String,
                                 processCode: String,
                                 labels: Labels = LabelCache(),
                                 backLink: Option[String] = None,
                                 answer: Option[String] = None)

case class PageContext(page: Page,
                       processStartUrl: Option[String],
                       processTitle: Text,
                       processId: String,
                       processCode: String,
                       labels: Labels = LabelCache(),
                       backLink: Option[String] = None,
                       answer: Option[String] = None)

object PageContext {
  def apply(pec: PageEvaluationContext, page: Page, updatedLabels: Labels): PageContext =
    PageContext(
      page,
      pec.processStartUrl,
      pec.processTitle,
      pec.processId,
      pec.processCode,
      updatedLabels,
      pec.backLink,
      pec.answer
    )
}
