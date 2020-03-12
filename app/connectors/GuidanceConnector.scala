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

import javax.inject.{Inject, Singleton}
import models.ocelot._
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GuidanceConnector @Inject() () {

  private[connectors] val stubbedProcess: Process = Json.parse(models.ocelot.PrototypeJson.json).as[Process]

  def getProcess(id: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Process] = Future.successful(stubbedProcess)
  def scratchProcess(guid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Process] = Future.successful(stubbedProcess)
}
