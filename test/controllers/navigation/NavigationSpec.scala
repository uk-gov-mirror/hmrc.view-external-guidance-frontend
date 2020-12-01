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

package controllers.navigation

import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest

import base.BaseSpec

class NavigationSpec extends BaseSpec {

  "Navigation object's method getPreviousPageByLink" should {

    "return true if the request's Url includes the query parameter p=1" in {

      implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/guidance/test/page-1?p=1")

      Navigation.getPreviousPageByLink() shouldBe true

    }

    "return false if the request's query string key does not match the expected query string" in {

      implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/guidance/test/page-3?f=x")

      Navigation.getPreviousPageByLink() shouldBe false
    }

    "return false if the request's query string value does not match the expected query string" in {

      implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/guidance/test/page-3?p=2")

      Navigation.getPreviousPageByLink() shouldBe false
    }

    "return false if a request does not have a query string" in {

      implicit val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", "/guidance/test/page-1")

      Navigation.getPreviousPageByLink() shouldBe false
    }

  }

}
