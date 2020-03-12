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
//import org.joda.time.{DateTime, DateTimeZone}
import play.api.libs.json.{Format, JsValue, Json}
import play.api.Logger
import models.ocelot._
import models.ocelot.stanzas._
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
  final case class Data(id: String, json: Process)

  object Data {
    implicit lazy val format: Format[Data] = ReactiveMongoFormats.mongoEntity {
      Json.format[Data]
    }
  }
}

@Singleton
class SessionRepository @Inject()(config: AppConfig,component: ReactiveMongoComponent)
                                 (implicit ec: ExecutionContext) extends ReactiveRepository[SessionRepository.Data, BSONObjectID](
  collectionName = "view-external-guidance-session",
  mongo = component.mongoConnector.db,
  domainFormat = SessionRepository.Data.format
) {

  // private val cacheTtl = config.get[Int]("parcels-cache.timeout")
  // private val indexName = "parcels-cache-ttl"

  // private val index = Index(
  //   key = Seq("lastUpdated" -> IndexType.Ascending),
  //   name = Some(indexName),
  //   options = BSONDocument("expireAfterSeconds" -> cacheTtl)
  // )

  // dropInvalidIndexes.flatMap {
  //   _ =>
  //     collection.indexesManager.ensure(index)
  // }

  //def get(key: String): Future[Option[JsValue]] = collection.find(Json.obj("_id" -> key), None).one[SessionRepository.Data].map(_.map(_.json))

  def set(key: String, value: Process): Future[WriteResult] = {
    val selector = Json.obj("_id" -> key)
    val document = Json.toJson(SessionRepository.Data(key, value))
    val modifier = Json.obj("$set" -> document)

    collection.update(false).one(selector, modifier, upsert = true)
  }

  // def invalidate(key: String): Future[FindAndModifyCommand.FindAndModifyResult] = {

  //   val selector = Json.obj("_id" -> key)

  //   collection.findAndRemove(selector)
  // }

  // private def dropInvalidIndexes: Future[_] = {
  //   collection.indexesManager.list().flatMap {
  //     indexes =>
  //       indexes.find {
  //         index =>
  //           index.name.contains(indexName) &&
  //             !index.options.getAs[Int]("expireAfterSeconds").contains(cacheTtl)
  //       }.map {
  //         _ =>
  //           cacheLogger.warn("dropping parcels-cache-ttl index as ttl value is incorrect")
  //           collection.indexesManager.drop(indexName)
  //       }.getOrElse(Future.successful(()))
  //   }
  // }
}
