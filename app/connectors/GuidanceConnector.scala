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
import uk.gov.hmrc.play.bootstrap.http.HttpClient
import config.AppConfig
import models.RequestOutcome
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class GuidanceConnector @Inject() (httpClient: HttpClient, appConfig: AppConfig) {

  private[connectors] val stubbedProcess: Process = Json.parse(models.ocelot.PrototypeJson.json).as[Process]

  def getProcess(id: String): Future[RequestOutcome[Process]] =
    Future.successful(Right(stubbedProcess))

  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/scratch/$uuid")

  def publishedProcess(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/published/$processId")

  def approvalProcess(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] =
    retrieveProcess(appConfig.externalGuidanceBaseUrl + s"/external-guidance/approval/$processId")

  private def retrieveProcess(endPoint: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[RequestOutcome[Process]] = {
    import connectors.httpParsers.GetProcessHttpParser.getProcessHttpReads

    httpClient.GET[RequestOutcome[Process]](endPoint, Seq.empty, Seq.empty)
  }

}
