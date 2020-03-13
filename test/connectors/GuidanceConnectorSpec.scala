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

package connectors

import base.BaseSpec
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class GuidanceConnectorSpec extends BaseSpec {

  "Calling the getProcess with an existing process ID" should {
    "return a model representing the Ocelot Process" in {
      val hc: HeaderCarrier = HeaderCarrier()
      val target = new GuidanceConnector()
      val expectProcess = Some(target.stubbedProcess)
      val result = target.getProcess("ext90002")(hc, implicitly)

      whenReady(result) { _ mustBe expectProcess }
    }
  }

  "Calling the scratchProcess with an existing scratch process UUID" should {

    "return a model representing the Ocelot Scatch Process" in {
      val hc: HeaderCarrier = HeaderCarrier()
      val target = new GuidanceConnector()
      val expectProcess = Some(target.scratchStubbedProcess)
      val result = target.scratchProcess("683d9aa0-2a0e-4e28-9ac8-65ce453d2730")(hc, implicitly)

      whenReady(result) { _ mustBe expectProcess }
    }
  }

}
