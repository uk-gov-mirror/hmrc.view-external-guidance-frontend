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

trait FormComponent extends UIComponent {
  val text: Text
  val hint: Option[Text]
  val body: Seq[UIComponent]
  val errorMsgs: Seq[ErrorMsg]
}

trait SequenceFormComponent extends FormComponent

trait NonExclusiveSequenceFormComponent extends SequenceFormComponent {
  val options: Seq[Text]
}

trait ExclusiveSequenceFormComponent extends NonExclusiveSequenceFormComponent {
  val exclusiveOption: Text
}