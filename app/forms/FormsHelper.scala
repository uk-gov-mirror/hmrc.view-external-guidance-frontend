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
import models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, Stanza, Question}

object FormsHelper {

  def bindFormData(inputStanza: Stanza, path: String)
                  (implicit request: Request[AnyContent]): Either[Form[_], (Form[_], List[String])] = {

    // Define form mapping for each data input type. Currently, the mapping is the same for
    // the single input data components. A different mapping will be required for dates.
    inputStanza match {
      case c: CurrencyInput => bindSingleInputValue(path -> nonEmptyText)
      case cpo: CurrencyPoundsOnlyInput => bindSingleInputValue(path -> nonEmptyText)
      case q: Question => bindSingleInputValue(path -> nonEmptyText)
    }

  }

  private def bindSingleInputValue(bindData: (String, Mapping[String]))
                                  (implicit request: Request[AnyContent]): Either[Form[_], (Form[_],List[String])] = {

    val formProvider: SingleAnswerFormProvider = new SingleAnswerFormProvider()

    val form = formProvider(bindData)

    form.bindFromRequest.fold(
      formWithErrors => Left(formWithErrors),
      formData => Right((form, List(formData)))
    )
  }

  private def bindTuple3Input(bindData: Seq[(String, Mapping[String])])
                             (implicit request: Request[AnyContent]): Either[Form[_], (Form[_], List[String])] = {

    val formProvider: Tuple3InputFormProvider = new Tuple3InputFormProvider()

    val form = formProvider(bindData)

    form.bindFromRequest.fold(
      formWithErrors => Left(formWithErrors),
      formData => Right((form, convertTuple3(formData)))
    )
  }

  private def convertTuple3(tuple: (Any, Any, Any)): List[String] =
    List( toStr(tuple._1), toStr(tuple._2), toStr(tuple._3))

  private def toStr(any: Any): String =

    any match {
      case s: String => s
      case _ => ""
    }

}
