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

// $COVERAGE-OFF$

package repositories

import config.AppConfig
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{Format, Json}
import models.ocelot._
import models.errors._
import models.MongoDateTimeFormats
import models.RequestOutcome
import play.modules.reactivemongo.ReactiveMongoComponent
import reactivemongo.bson.{BSONDocument, BSONObjectID}
import reactivemongo.play.json.ImplicitBSONHandlers.JsObjectDocumentWriter
import java.time.Instant
import uk.gov.hmrc.mongo.ReactiveRepository
import uk.gov.hmrc.mongo.json.ReactiveMongoFormats
import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.api.indexes.IndexType
import reactivemongo.api.indexes.Index
import reactivemongo.bson.BSONInteger

object DefaultSessionRepository {
  final case class SessionProcess(id: String, processId: String, process: Process, answers: Map[String, String], pageHistory: List[String], lastAccessed: Instant)

  object SessionProcess {
    implicit val dateFormat: Format[Instant] = MongoDateTimeFormats.instantFormats
    implicit lazy val format: Format[SessionProcess] = ReactiveMongoFormats.mongoEntity { Json.format[SessionProcess] }
  }
}

case class ProcessContext(process: Process, answers: Map[String, String], backLink: Option[String])

trait SessionRepository {
  def get(key: String, pageUrl: String): Future[RequestOutcome[ProcessContext]]
  def set(key: String, process: Process): Future[RequestOutcome[Unit]]
  def saveAnswerToQuestion(key: String, url: String, answers: String): Future[RequestOutcome[Unit]]
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

  def get(key: String, pageUrl: String): Future[RequestOutcome[ProcessContext]] =
    findAndUpdate(Json.obj("_id" -> key),
                  Json.obj(
                    "$set" -> Json.obj(ttlExpiryFieldName -> Json.obj("$date" -> Instant.now().toEpochMilli)),
                    "$push" -> Json.obj("pageHistory" -> pageUrl)
                  ),
                  fetchNewObject = false
    ).flatMap { r =>
          r.result[DefaultSessionRepository.SessionProcess]
          .fold {
            logger.warn(s"Attempt to retrieve cached process from session repo with _id=$key returned no result, lastError ${r.lastError}")
            Future.successful(Left(NotFoundError): RequestOutcome[ProcessContext])
          }{ sp =>
            //
            // pageHistory returned by findAndUpdate() is intentionally the history before the update!!
            //
            sp.pageHistory.reverse match {
              case Nil => Future.successful(Right(ProcessContext(sp.process, sp.answers, None)))
              case x :: xs if x == pageUrl =>
                // REFRESH: Rewrite pageHistory as current history without current page just added, Back link is x
                savePageHistory(key, sp.pageHistory).map {
                  case Left(err) =>
                    logger.error(s"Unable to save backlink history, error = $err")
                    Right(ProcessContext(sp.process, sp.answers, xs.headOption))
                  case _ => Right(ProcessContext(sp.process, sp.answers, xs.headOption))
                }
              case x :: y :: xs if y == pageUrl =>
                // BACK: pageHistory becomes y :: xs and backlink is head of xs
                savePageHistory(key, (y :: xs).reverse).map {
                  case Left(err) =>
                    logger.error(s"Unable to save backlink history, error = $err")
                    Right(ProcessContext(sp.process, sp.answers, xs.headOption))
                  case _ => Right(ProcessContext(sp.process, sp.answers, xs.headOption))
                }
              case x :: xs =>
                // FORWARD: Back link x, pageHistory intact
                Future.successful(Right(ProcessContext(sp.process, sp.answers, Some(x))))
            }
          }
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to retrieve process from session repo with _id=$key")
          Left(DatabaseError)
      }

  def set(key: String, process: Process): Future[RequestOutcome[Unit]] = {
    val sessionDocument = Json.toJson(DefaultSessionRepository.SessionProcess(key, process.meta.id, process, Map(), Nil, Instant.now))
    collection
      .update(false)
      .one(Json.obj("_id" -> key), Json.obj("$set" -> sessionDocument), upsert = true)
      .map(_ => Right(()))
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to persist process=${process.meta.id} to session repo using _id=$key")
          Left(DatabaseError)
      }
  }

  def saveAnswerToQuestion(key: String, url: String, answer: String): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> key),
      Json.obj("$set" -> Json.obj(ttlExpiryFieldName -> Json.obj("$date" -> Instant.now().toEpochMilli), s"answers.$url" -> answer))
    ).map { result =>
        result
          .result[DefaultSessionRepository.SessionProcess]
          .fold {
            logger.warn(
              s"Attempt to saveAnswerToQuestion using _id=$key returned no result, lastError ${result.lastError}, url: $url, answer: $answer"
            )
            Left(NotFoundError): RequestOutcome[Unit]
          }(_ => Right({}))
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to update question answers within session repo with _id=$key, url: $url, answer: $answer")
          Left(DatabaseError)
      }

  private def savePageHistory(key: String, pageHistory: List[String]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> key),
      Json.obj("$set" -> Json.obj(ttlExpiryFieldName -> Json.obj("$date" -> Instant.now().toEpochMilli), s"pageHistory" -> pageHistory))
    ).map { result =>
        result
          .result[DefaultSessionRepository.SessionProcess]
          .fold {
            logger.warn(
              s"Attempt to savePageHistory using _id=$key eturned no result, lastError ${result.lastError}"
            )
            Left(NotFoundError): RequestOutcome[Unit]
          }(_ => Right({}))
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to savePageHistory to session repo with _id=$key")
          Left(DatabaseError)
      }

}
