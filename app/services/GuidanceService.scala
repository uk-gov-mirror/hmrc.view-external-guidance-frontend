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

@Singleton
class GuidanceService @Inject() (connector: GuidanceConnector, sessionRepository: SessionRepository) {

  // def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
  //   for{
  //     processOption <- connector.scratchProcess(uuid)
  //     res <- sessionRepository.set(hc.sessionId.fold(uuid)(_.value), processOption.get)
  //   } yield PageBuilder.pages(processOption.get).fold(_ => "",pages => pages.head.url)


  def scratchProcess(uuid: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
    connector.scratchProcess(uuid).flatMap{
      _.map{process =>
        Future.successful{
          val key = hc.sessionId.fold(uuid)(_.value)
          sessionRepository.set(key, process)
          PageBuilder.pages(process).fold(_ => "",pages => pages.head.url)
        }
      }.getOrElse(Future.successful(""))
    }

  def getStartPageUrl(processId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[String] =
    connector.getProcess(processId) map { process =>
      println(s"SAVING ${process.meta}")
      sessionRepository.set(processId, process)
      PageBuilder.pages(process) match {
        case Right(pages) =>
          val startPage = pages.find(page => page.id == "start")
          startPage.fold("")(page => page.url)
        case _ => ""
      }
    }

  def getPage(url: String, sessionId: String)(implicit hc: HeaderCarrier, context: ExecutionContext): Future[Option[Page]] =
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
