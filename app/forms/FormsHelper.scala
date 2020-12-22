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
import models.ocelot.stanzas.{DateInput => OcelotDateInput, DataInput => OcelotDataInput, Question => OcelotQuestion}
import models.ui.DateInput.partitionSubmittedDateAnswer
import models.ui.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Question, UIComponent}
import models.ui.{SubmittedAnswer, SubmittedDateAnswer, SubmittedTextAnswer}

object FormsHelper {

  def bindFormData(inputStanza: OcelotDataInput, path: String)
                  (implicit request: Request[_]): Either[Form[_], (Form[_], SubmittedAnswer)] = {

    // Define form mapping for each data input type
    inputStanza match {
      case c: OcelotCurrencyInput => bindSubmittedTextAnswer(path -> nonEmptyText)
      case cpo: OcelotCurrencyPoundsOnlyInput => bindSubmittedTextAnswer(path -> nonEmptyText)
      case q: OcelotQuestion => bindSubmittedTextAnswer(path -> nonEmptyText)
      case d: OcelotDateInput => bindSubmittedDateAnswer()
    }

  }

  def populateForm(input: UIComponent, path: String, answer: Option[String]): Form[_] = {

    input match {
      case c: CurrencyInput => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case cpo: CurrencyPoundsOnlyInput => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case q: Question => populateSubmittedTextAnswerForm(path -> nonEmptyText, path, answer)
      case d: DateInput => populateSubmittedDateAnswerForm(answer)
    }

  }


  private def bindSubmittedTextAnswer(bindData: (String, Mapping[String]))
                                  (implicit request: Request[_]): Either[Form[_], (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form = formProvider(bindData)

    form.bindFromRequest().fold(
      formWithErrors => Left(formWithErrors),
      formData => Right((form.fill(formData), formData))
    )

  }

  private def bindSubmittedDateAnswer()
                                     (implicit request: Request[_] ): Either[Form[_], (Form[_], SubmittedAnswer)] = {

    val formProvider: SubmittedDateAnswerFormProvider = new SubmittedDateAnswerFormProvider()

    val form = formProvider()

    form.bindFromRequest().fold(
      formWithErrors => Left(formWithErrors),
      formData => Right((form.fill(formData), formData))
    )

  }

  private def populateSubmittedTextAnswerForm(bindData: (String, Mapping[String]), path: String, answer: Option[String]): Form[SubmittedTextAnswer] = {

    val formProvider: SubmittedTextAnswerFormProvider = new SubmittedTextAnswerFormProvider()

    val form: Form[SubmittedTextAnswer] = formProvider(bindData)

    answer match {
      case Some(value) => form.bind(Map(path -> value))
      case None => form
    }

  }

  private def populateSubmittedDateAnswerForm(answer: Option[String]): Form[SubmittedDateAnswer] = {

    val formProvider: SubmittedDateAnswerFormProvider = new SubmittedDateAnswerFormProvider()

    val form: Form[SubmittedDateAnswer] = formProvider()

    answer match {
      case Some(value) =>

        val (day, month, year) = partitionSubmittedDateAnswer(value)

        form.bind(
          Map(
          "day" -> day,
          "month" -> month,
          "year" -> year
             )
        )

      case None => form
    }
  }

}
