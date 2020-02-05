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

import base.BaseSpec

class UIComponentsSpec extends BaseSpec {

  "UIComponents" must {

    "build into a page of sequenced components with an associated url path" in {

      val txt1 = Text(Vector("Hello my name is ....","Welsh, Hello my name is ...."))
      val txt2 = Text(Vector(" and today is Wednesday","Welsh,  and today is Wednesday"))
      val link1 = EmbeddedLink("/secondpage",Vector("Unsure?", "Welsh, Unsure?"))
      val para = Paragraph(Seq(txt1))
      val paraItems = Seq(txt1,link1,txt2)
      val paraWithLink = Paragraph(paraItems)

      val answer1 = Answer(Vector("Not bad", "Welsh, Not bad"), Some(Vector("you think this is not bad", "Welsh, you think this is not bad")), "/firstpage/notbad")
      val answer2 = Answer(Vector("ok","Welsk, ok"), Some(Vector("you think this is not bad", "Wels,you think this is not bad")), "/firstpage/notbad")
      val answers = AnswerGroup(Seq(answer1, answer2), false)
      val question = Question(Vector("What do you think of this example","Welsh, What do you think of this example"), None, answers)

      val components = Seq(para, paraWithLink, question)
      val page = Page("/firstpage", components)
      val langIndex = 0
      page.components.foreach{ cmp =>
        cmp match {
          case p:Paragraph =>
            println(p.items.mkString(","))
          case q: Question =>
            println(s"QUESTION: ${q.txt(langIndex)}")
            q.answerGroup.answers.foreach(println)
        }
      }
    }
  }
}
