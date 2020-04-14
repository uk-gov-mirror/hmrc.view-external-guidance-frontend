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

// $COVERAGE-OFF$Problem

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, Json}
import models.ocelot._
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import org.joda.time.{DateTime, DateTimeZone}
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.indexes.IndexType
import reactivemongo.api.indexes.Index

object DefaultSessionRepository {
  final case class SessionProcess(id: String, processId: String, process: Process, lastAccessed: DateTime)

  object SessionProcess {
    implicit val dateFormat: Format[DateTime] = ReactiveMongoFormats.dateTimeFormats
    implicit lazy val format: Format[SessionProcess] = ReactiveMongoFormats.mongoEntity { Json.format[SessionProcess] }
  }
}

trait SessionRepository {
  def get(key: String): Future[Option[Process]]
  def set(key: String, process: Process): Future[Option[Unit]]
}

@Singleton
class DefaultSessionRepository @Inject() (config: AppConfig, component: ReactiveMongoComponent)(implicit ec: ExecutionContext)
    extends ReactiveRepository[DefaultSessionRepository.SessionProcess, BSONObjectID](
      collectionName = "view-external-guidance-session",
      mongo = component.mongoConnector.db,
      domainFormat = DefaultSessionRepository.SessionProcess.format
    )
    with SessionRepository {

  override def indexes: Seq[Index] = Seq(
    Index(Seq("lastAccessed" -> IndexType.Ascending), 
          name = Some("lastAccessedIndex"), 
          options = BSONDocument("expireAfterSeconds" -> config.sessionProcessTTLMinutes * 60))
  )      

  def get(key: String): Future[Option[Process]] = {
    updateAccessTime(key)
    collection
      .find(Json.obj("_id" -> key), None)
      .one[DefaultSessionRepository.SessionProcess]
      .map(_.map(_.process))
  }

  def set(key: String, process: Process): Future[Option[Unit]] = {
    val selector = Json.obj("_id" -> key)
    val document = Json.toJson(DefaultSessionRepository.SessionProcess(key, process.meta.id, process, DateTime.now(DateTimeZone.UTC)))
    val modifier = Json.obj("$set" -> document)

    collection.update(false).one(selector, modifier, upsert = true) map (_ => Some(())) recover {
      case lastError =>
        logger.error(s"Unable to persist process=${process.meta.id} against id=$key")
        None
    }
  }

  def updateAccessTime(key: String): Future[Option[Unit]] = {
    val selector = Json.obj("_id" -> key)
    val modifier = Json.obj("$set" -> Json.obj("lastAccessed" -> Json.obj("$date" -> DateTime.now(DateTimeZone.UTC).getMillis)))
    collection.update(false).one(selector, modifier) map (_ => Some(())) recover {
      case lastError =>
        logger.error(s"Unable to update lastAccessed time associated with key=$key")
        None
    }
  }
}
