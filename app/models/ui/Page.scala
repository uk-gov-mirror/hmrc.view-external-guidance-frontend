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

import scala.annotation.tailrec

case class Page(urlPath: String, components: Seq[UIComponent]) {

  @tailrec
  private def findFirstH1Text(c: Seq[UIComponent]): Option[Text] =
    c match {
      case Nil => None
      case x :: xs => x match {
        case h:H1 => Some(h.txt)
        case _ => findFirstH1Text(xs)
      }
    }

  val heading: Text = findFirstH1Text(components).getOrElse(Text("",""))
}
