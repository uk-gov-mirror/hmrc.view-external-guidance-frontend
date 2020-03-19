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
import models.RequestOutcome
import models.ocelot._
import play.api.libs.json.Json
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import config.AppConfig


import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GuidanceConnector @Inject() (httpClient: HttpClient, appConfig: AppConfig) {

  private[connectors] val stubbedProcess: Process = Json.parse(models.ocelot.PrototypeJson.json).as[Process]

  def getProcess(id: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Process]] =
    Future.successful(Some(stubbedProcess))

  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Process]] = {
    import connectors.httpParsers.GetScratchProcessHttpParser.getScratchProcessHttpReads
    val endpoint: String = appConfig.externalGuidanceBaseUrl + s"/external-guidance/scratch/$uuid"

    httpClient.GET[RequestOutcome[Process]](endpoint, Seq.empty, Seq.empty).map{
      case Right(process) => Some(process)
      case Left(err) => None
    }
  }
}
