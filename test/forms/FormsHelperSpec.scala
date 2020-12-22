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
import play.api.data.Form
import uk.gov.hmrc.http.SessionKeys
import models.ocelot.Phrase
import models.ocelot.stanzas.Question
import models.ui.{SubmittedAnswer, SubmittedTextAnswer}

import play.api.test.FakeRequest

import base.BaseSpec

class FormsHelperSpec extends BaseSpec {

  val processId: String = "ext90000"
  val path: String = s"/guidance/$processId/question"
  val relativePath: String = path.drop(1)
  val questionAnswer: String = "0"

  "FormsHelper's binding functionality" should {

    "be able to bind data from a question input component" in {

      implicit val request: Request[_] = FakeRequest("POST", path)
        .withSession(SessionKeys.sessionId -> processId)
        .withFormUrlEncodedBody(relativePath -> questionAnswer)

      val question: Question = Question(
        Phrase("Which way is the artic?", "Welsh, which way is the arctic?"),
        Seq(
          Phrase("North", "Welsh, North"),
          Phrase("South", "Welsh, South")
        ),
        Seq("2", "3"),
        None,
        false
      )

      val result: Either[Form[_], (Form[_], SubmittedAnswer)] = FormsHelper.bindFormData(question, relativePath)
    }
  }

}
