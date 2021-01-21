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

package forms

import play.api.mvc._
import play.api.data.Form
import core.models.ocelot.Phrase
import core.models.ocelot.stanzas.{CurrencyInput, CurrencyPoundsOnlyInput, DateInput, Question}
import models.ui.SubmittedAnswer

import play.api.test.FakeRequest

import base.BaseSpec

class FormsHelperSpec extends BaseSpec {

  private trait Test {

    val processId: String = "ext90000"
    val path: String = s"/guidance/$processId/question"
    val relativePath: String = path.drop(1)
    val incorrectPath: String = "guidance/somePath"
    val questionAnswer: String = "0"
    val currencyAnswer: String = "10.00"
    val currencyPoundsOnlyAnswer: String = "10"
    val dayAnswer: String = "16"
    val monthAnswer: String = "5"
    val yearAnswer: String = "2019"
    val dateAnswer: String = dayAnswer + "/" + monthAnswer + "/" + yearAnswer
    val expectedErrorMessage: String = "error.required"

    val question: Question = Question(
      Phrase("Which way is the artic?", "Welsh, which way is the arctic?"),
      Seq(
        Phrase("North", "Welsh, North"),
        Phrase("South", "Welsh, South")
      ),
      Seq("2", "3"),
      None,
      stack = false
    )

    val currencyInput: CurrencyInput = CurrencyInput(
      Seq("4"),
      Phrase("How much money do you have?", "Welsh, How much money do you have?"),
      None,
      "moneyInHand",
      None,
      stack = false
    )

    val currencyPoundsOnlyInput: CurrencyPoundsOnlyInput = CurrencyPoundsOnlyInput(
      Seq("5"),
      Phrase("How much money do you have rounded to the nearest pound?", "Welsh, How much money do you have rounded to the nearest pound?"),
      None,
      "roundedMoneyInHand",
      None,
      stack = false
    )

    val dateInput: DateInput = DateInput(
      Seq("25"),
      Phrase("Enter a random date", "Welsh, Enter a random date"),
      None,
      "randomDate",
      None,
      stack = false
    )

  }

  "FormsHelper's binding functionality" should {

    "be able to bind data for a question input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> questionAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(question, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(questionAnswer)
          submittedAnswer.text shouldBe questionAnswer
        case Left(formWithErrors: Form[_]) => fail("Form binding returned a form with errors")
      }
    }

    "be able to bind data for a currency input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> currencyAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(currencyAnswer)
          submittedAnswer.text shouldBe currencyAnswer
        case Left(formWithErrors: Form[_]) => fail("Form binding returned a form with errors")
      }

    }

    "be able to bind data for a currency pounds only input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(relativePath -> currencyPoundsOnlyAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyPoundsOnlyInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form(relativePath).value shouldBe Some(currencyPoundsOnlyAnswer)
          submittedAnswer.text shouldBe currencyPoundsOnlyAnswer
        case Left(formWithErrors: Form[_]) => fail("Form binding returned a form with errors")
      }

    }

    "be able to bind data for a date input component" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "month" -> monthAnswer, "year" -> yearAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Right((form: Form[_], submittedAnswer: SubmittedAnswer)) =>
          form("day").value shouldBe Some(dayAnswer)
          form("month").value shouldBe Some(monthAnswer)
          form("year").value shouldBe Some(yearAnswer)
          submittedAnswer.text shouldBe dateAnswer
        case Left(formWithErrors: Form[_]) => fail("Form binding returned a form with errors")
      }

    }

    "return a form with errors if the question answer is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> questionAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(question, relativePath)

      result match {
        case Left(formWithErrors: Form[_]) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
        case _ => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with errors if the currency value is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> currencyAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyInput, relativePath)

      result match {
        case Left(formWithErrors: Form[_]) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
        case _ => fail("Binding should fail for incorrect request data")
      }

    }

    "return a form with errors if the currency pounds only value is not mapped to the correct key in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody(incorrectPath -> currencyPoundsOnlyAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(currencyPoundsOnlyInput, relativePath)

      result match {
        case Left(formWithErrors: Form[_]) =>
          formWithErrors.errors.head.key shouldBe relativePath
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
        case _ => fail("Binding should fail for incorrect request data")
      }
    }

    "return a form with errors if a date is not fully defined in the request" in new Test {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withFormUrlEncodedBody("day" -> dayAnswer, "year" -> yearAnswer)

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(dateInput, relativePath)

      result match {
        case Left(formWithErrors: Form[_]) =>
          formWithErrors.errors.head.key shouldBe "month"
          formWithErrors.errors.head.message shouldBe expectedErrorMessage
        case _  => fail("Binding should fail for incorrect request data")
      }

    }

    "populate a form with the submitted answer for a question" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(question, relativePath, Some(questionAnswer))

      form(relativePath).value shouldBe Some(questionAnswer)

    }

    "populate a form with the submitted answer for a currency value" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(currencyInput, relativePath, Some(currencyAnswer))

      form(relativePath).value shouldBe Some(currencyAnswer)

    }

    "populate a form with the submitted answer for a currency pounds only value" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(currencyPoundsOnlyInput, relativePath, Some(currencyPoundsOnlyAnswer))

      form(relativePath).value shouldBe Some(currencyPoundsOnlyAnswer)

    }

    "populate a form with the submitted answer for a date" in new Test {

      val form: Form[_] = FormsHelper.populatedForm(dateInput, relativePath, Some(dateAnswer))

      form("day").value shouldBe Some(dayAnswer)
      form("month").value shouldBe Some(monthAnswer)
      form("year").value shouldBe Some(yearAnswer)
    }

  }

}
