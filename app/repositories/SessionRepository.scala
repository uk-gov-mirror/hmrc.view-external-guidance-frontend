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
import models.errors._
import models.RequestOutcome
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import org.joda.time.{DateTime, DateTimeZone}
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.indexes.IndexType
import reactivemongo.api.indexes.Index
import reactivemongo.bson.BSONInteger

object DefaultSessionRepository {
  final case class SessionProcess(id: String, processId: String, process: Process, lastAccessed: DateTime)

  object SessionProcess {
    implicit val dateFormat: Format[DateTime] = ReactiveMongoFormats.dateTimeFormats
    implicit lazy val format: Format[SessionProcess] = ReactiveMongoFormats.mongoEntity { Json.format[SessionProcess] }
  }
}

trait SessionRepository {
  def get(key: String): Future[RequestOutcome[Process]]
  def set(key: String, process: Process): Future[RequestOutcome[Unit]]
}

@Singleton
class DefaultSessionRepository @Inject() (config: AppConfig, component: ReactiveMongoComponent)(implicit ec: ExecutionContext)
    extends ReactiveRepository[DefaultSessionRepository.SessionProcess, BSONObjectID](
      collectionName = "view-external-guidance-session",
      mongo = component.mongoConnector.db,
      domainFormat = DefaultSessionRepository.SessionProcess.format
    )
    with SessionRepository {

  val lastAccessedIndexName = "lastAccessedIndex"
  val expiryAfterOptionName = "expireAfterSeconds"
  val ttlExpiryFieldName = "lastAccessed"

  override def ensureIndexes(implicit ec: ExecutionContext): Future[Seq[Boolean]] =
    // If current configuration includes an update to the expiry period of the TTL index, drop the current index to allow its re-creation
    collection.indexesManager.list().flatMap { indexes =>
      indexes
        .filter(idx =>
          idx.name == Some(lastAccessedIndexName) &&
          idx.options.getAs[BSONInteger](expiryAfterOptionName).fold(false)(_.as[Int] != config.timeoutInSeconds)
        )
        .map { _ =>
          logger.warn(s"Dropping $lastAccessedIndexName ready for re-creation, due to configured timeout change")
          collection.indexesManager.drop(lastAccessedIndexName).map(ret => logger.info(s"Drop of $lastAccessedIndexName index returned $ret"))
        }

      super.ensureIndexes
    }

  override def indexes: Seq[Index] = {
    logger.info(s"SessionRepository TTL set to ${config.timeoutInSeconds} seconds")
    Seq(
      Index(
        Seq(ttlExpiryFieldName -> IndexType.Ascending),
        name = Some(lastAccessedIndexName),
        options = BSONDocument(expiryAfterOptionName -> config.timeoutInSeconds)
      )
    )
  }

  def get(key: String): Future[RequestOutcome[Process]] = {
    val updateModifier = Json.obj("$set" -> Json.obj(ttlExpiryFieldName -> Json.obj("$date" -> DateTime.now(DateTimeZone.UTC).getMillis)))
    findAndUpdate(Json.obj("_id" -> key), updateModifier, fetchNewObject = false)
      .map(wr => wr.result[DefaultSessionRepository.SessionProcess].fold(Left(DatabaseError): RequestOutcome[Process])(r => Right(r.process)))
      .recover {
        case lastError =>
          logger.error(s"Unable to retrieve process associated with key=$key")
          Left(DatabaseError)
      }
  }

  def set(key: String, process: Process): Future[RequestOutcome[Unit]] = {
    val sessionDocument = Json.toJson(DefaultSessionRepository.SessionProcess(key, process.meta.id, process, DateTime.now(DateTimeZone.UTC)))

    collection
      .update(false)
      .one(Json.obj("_id" -> key), Json.obj("$set" -> sessionDocument), upsert = true)
      .map(_ => Right(()))
      .recover {
        case lastError =>
          logger.error(s"Unable to persist process=${process.meta.id} against id=$key")
          Left(DatabaseError)
      }
  }

}
