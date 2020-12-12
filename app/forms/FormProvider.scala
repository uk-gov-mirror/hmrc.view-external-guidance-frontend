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

import play.api.data.Forms.{single, tuple}
import play.api.data.{Form, Mapping}

trait FormProvider

class SingleAnswerFormProvider extends FormProvider {

  def apply(bindData: (String, Mapping[String])): Form[String] =

    Form(
      single(
        bindData._1-> bindData._2
      )
    )

}

class Tuple3InputFormProvider extends FormProvider {

  def apply(bindData: Seq[(String, Mapping[String])]): Form[(String, String, String)] =

    Form(
      tuple(
        bindData.head._1 -> bindData.head._2,
        bindData(1)._1 -> bindData(1)._2,
        bindData.last._1 -> bindData.last._2
      )
    )

}


