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

import play.api.mvc._
import play.api.data.{Form, Mapping}
import play.api.data.Forms.nonEmptyText
import models.ocelot.stanzas.{CurrencyInput => OcelotCurrencyInput, CurrencyPoundsOnlyInput => OcelotCurrencyPoundsOnlyInput}
import models.ocelot.stanzas.{DataInput => OcelotDataInput, Question => OcelotQuestion}
import models.ui.{CurrencyInput, CurrencyPoundsOnlyInput, Question, UIComponent}
import models.ui.{SubmittedAnswer, SubmittedDateAnswer, SubmittedTextAnswer}

object FormsHelper {

  def bindFormData(inputStanza: OcelotDataInput, path: String)
                  (implicit request: Request[AnyContent]): Either[Form[_], (Form[_], SubmittedAnswer)] = {

    // Define form mapping for each data input type. Currently, the mapping is the same for
    // the single input data components. A different mapping will be required for dates.
    inputStanza match {
      case c: OcelotCurrencyInput => bindSingleTextAnswer(path -> nonEmptyText)
      case cpo: OcelotCurrencyPoundsOnlyInput => bindSingleTextAnswer(path -> nonEmptyText)
      case q: OcelotQuestion => bindSingleTextAnswer(path -> nonEmptyText)
    }

  }

  def populateForm(input: UIComponent, path: String, answer: Option[String]): Form[_] = {

    input match {
      case c: CurrencyInput => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case cpo: CurrencyPoundsOnlyInput => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case q: Question => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
    }

  }


  private def bindSingleTextAnswer(bindData: (String, Mapping[String]))
                                  (implicit request: Request[AnyContent]): Either[Form[_], (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form = formProvider(bindData)

    form.bindFromRequest()fold(
      formWithErrors => Left(formWithErrors),
      formData => Right((form, formData))
    )

  }

  private def populateSubmittedTextAnswerForm(bindData: (String, Mapping[String]), path: String, answer: Option[String]): Form[SubmittedTextAnswer] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form = formProvider(bindData)

    answer match {
      case Some(value) => form.bind(Map(path -> value))
      case None => form
    }

  }

}
