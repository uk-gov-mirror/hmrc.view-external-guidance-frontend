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

package services

import connectors.GuidanceConnector
import javax.inject.{Inject, Singleton}
import models.ui.Page
import uk.gov.hmrc.http.HeaderCarrier
import play.api.Logger
import scala.util.{Success, Failure}
import scala.concurrent.{ExecutionContext, Future}
import repositories.SessionRepository
import models.ocelot.Process

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector, sessionRepository: SessionRepository) {

  private def startProcessView( id: String, processById: String => Future[Option[Process]])(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] = {
    processById(id).flatMap{
      _.map{process =>
          sessionRepository.set(hc.sessionId.fold(id)(_.value), process).map{ result =>
            if (result) PageBuilder.pages(process).fold(_ => "",pages => pages.head.url) else ""
          }          
      }.getOrElse(Future.successful(""))
    }
  }

  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
    startProcessView(uuid, connector.scratchProcess)

  def getStartPageUrl(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
    startProcessView(processId, connector.getProcess)

  def getPage(url: String, sessionId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Page]] = {
    sessionRepository.get(sessionId).flatMap{ 
      _.map{ process =>
          Future.successful(
            PageBuilder.pages(process).fold( _ => None, pages => {
                val stanzaIdToUrlMap: Map[String, String] = pages.map(page => (page.id, s"/guidance${page.url}")).toMap
                pages.find(page => page.url == url)
                     .map( pge => UIBuilder.fromStanzaPage(pge)(stanzaIdToUrlMap))
              }
            )
          )
      }.getOrElse(Future.successful(None))
    }
  }

}
