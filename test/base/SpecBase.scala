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

package base

import org.scalatest.{WordSpec, MustMatchers}
import play.api.libs.json._

trait SpecBase extends WordSpec with MustMatchers {

  def missingJsObjectAttrTests[T](jsObject: JsObject, attrsToIgnore: List[String] = Nil)
                                 (implicit objectReads:Reads[T]):Unit =
    jsObject.keys.filterNot(attrsToIgnore.contains(_)).foreach { attributeName =>
      s"throw exception when json is missing attribute $attributeName" in {
        val invalidJson = jsObject - attributeName
        invalidJson.validate[T] match {
          case JsSuccess(_, _) => fail(s"Object incorrectly created when attribute $attributeName missing")
          case JsError(_) => succeed
        }
      }
    }
}
