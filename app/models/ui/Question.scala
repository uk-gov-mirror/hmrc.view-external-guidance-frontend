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

trait FormComponent extends UIComponent {
  val text: Text
  val hint: Option[Text]
  val body: Seq[UIComponent]
  val errors: Seq[ErrorMsg]
  val errorMsgs: Seq[ValueErrorMsg] = errors.collect{case e: ValueErrorMsg => e}
  val existError: Option[RequiredErrorMsg] = errors.collect{case e: RequiredErrorMsg => e}.headOption
  val typeError: Option[TypeErrorMsg] = errors.collect{case e: TypeErrorMsg => e}.headOption
}

case class Answer(text: Text, hint: Option[Text]) extends UIComponent

case class Question(
  text: Text,
  hint: Option[Text],
  body: Seq[UIComponent],
  answers: Seq[Answer],
  errors: Seq[ErrorMsg] = Nil) extends FormComponent {
  val horizontal: Boolean = answers.length == 2 &&
                            answers.forall(ans => ans.hint.isEmpty &&
                                                  ans.text.english.map(_.toWords.length).sum == 1)

}
