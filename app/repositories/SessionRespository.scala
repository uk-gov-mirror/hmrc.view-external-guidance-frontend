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

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, JsValue, Json}
import play.api.Logger
import models.ocelot._
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import reactivemongo.play.json.collection.JSONBatchCommands.FindAndModifyCommand
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats

import scala.concurrent.{ExecutionContext, Future}

object SessionRepository {
  final case class Data(id: String, process: Process)

  object Data {
    implicit lazy val format: Format[Data] = ReactiveMongoFormats.mongoEntity { Json.format[Data] }
  }
}

@Singleton
class SessionRepository @Inject()(config: AppConfig, component: ReactiveMongoComponent)
                                 (implicit ec: ExecutionContext) extends ReactiveRepository[SessionRepository.Data, BSONObjectID](
                                                                            collectionName = "view-external-guidance-session",
                                                                            mongo = component.mongoConnector.db,
                                                                            domainFormat = SessionRepository.Data.format
                                                                         ) {
  def get(key: String): Future[Option[Process]] =
    collection.find(Json.obj("_id" -> key), None)
              .one[SessionRepository.Data]
              .map(_.map(_.process))

  def set(key: String, value: Process): Future[Boolean] = {
    val selector = Json.obj("_id" -> key)
    val document = Json.toJson(SessionRepository.Data(key, value))
    val modifier = Json.obj("$set" -> document)

    collection.update(false).one(selector, modifier, upsert = true) map {
      case _ => true
    } recover {
      case lastError => false
    }
  }
}
