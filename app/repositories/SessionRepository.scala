/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.libs.json.{Format, Json, Writes}
import core.models.ocelot._
import core.models.ocelot.stanzas.Stanza
import core.models.errors._
import core.models.MongoDateTimeFormats
import core.models.RequestOutcome
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
  final case class SessionProcess(id: String,
                                  processId: String,
                                  process: Process,
                                  labels: Map[String, Label],
                                  flowStack: List[FlowStage],
                                  stanzaPool: Map[String, Stanza],
                                  urlToPageId: Map[String, String],
                                  answers: Map[String, String],
                                  pageHistory: List[String],
                                  lastAccessed: Instant)

  object SessionProcess {
    implicit val dateFormat: Format[Instant] = MongoDateTimeFormats.instantFormats
    implicit lazy val format: Format[SessionProcess] = ReactiveMongoFormats.mongoEntity { Json.format[SessionProcess] }
  }
}

case class ProcessContext(process: Process,
                          answers: Map[String, String],
                          labels: Map[String, Label],
                          flowStack: List[FlowStage],
                          stanzaPool: Map[String, Stanza],
                          urlToPageId: Map[String, String],
                          backLink: Option[String]) {
  val secure: Boolean = process.flow.get(SecuredProcess.PassPhrasePageId).fold(true){_ =>
    labels.get(SecuredProcess.PassPhraseResponseLabelName).fold(false)(lbl => lbl.english.headOption == process.passPhrase)
  }
}

trait SessionRepository {
  def get(key:String): Future[RequestOutcome[ProcessContext]]
  def get(key: String, pageHistoryUrl: Option[String], previousPageByLink: Boolean): Future[RequestOutcome[ProcessContext]]
  def set(key: String, process: Process, urlToPageId: Map[String, String]): Future[RequestOutcome[Unit]]
  def saveFormPageState(key: String, url: String, answer: String, labels: Labels): Future[RequestOutcome[Unit]]
  def savePageState(key: String, labels: Labels): Future[RequestOutcome[Unit]]
}

@Singleton
class DefaultSessionRepository @Inject() (config: AppConfig, component: ReactiveMongoComponent)(implicit ec: ExecutionContext)
  extends ReactiveRepository[DefaultSessionRepository.SessionProcess, BSONObjectID](
    collectionName = "view-external-guidance-session",
    mongo = component.mongoConnector.db,
    domainFormat = DefaultSessionRepository.SessionProcess.format
  )
  with SessionRepository {
  private type FieldAttr = (String, Json.JsValueWrapper)
  val lastAccessedIndexName = "lastAccessedIndex"
  val expiryAfterOptionName = "expireAfterSeconds"
  val ttlExpiryFieldName = "lastAccessed"

  override def ensureIndexes(implicit ec: ExecutionContext): Future[Seq[Boolean]] =
    // If current configuration includes an update to the expiry period of the TTL index, drop the current index to allow its re-creation
    collection.indexesManager.list().flatMap { indexes =>
      indexes
        .filter(idx =>
          idx.name.contains(lastAccessedIndexName) &&
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

  def get(key:String): Future[RequestOutcome[ProcessContext]] =
    find("_id" -> key).map {
      case Nil =>  Left(NotFoundError)
      case r :: _ => Right(ProcessContext(r.process, r.answers, r.labels, r.flowStack, r.stanzaPool, r.urlToPageId, None))
    }.recover {
      case lastError =>
      logger.error(s"Error $lastError occurred in method get(key: String) attempting to retrieve session $key")
      Left(DatabaseError)
    }

  def get(key: String, pageHistoryUrl: Option[String], previousPageByLink: Boolean): Future[RequestOutcome[ProcessContext]] = {
    findAndUpdate(Json.obj("_id" -> key),
                  Json.obj((List(toFieldPair("$set", Json.obj(toFieldPair(ttlExpiryFieldName, Json.obj("$date" -> Instant.now().toEpochMilli))))) ++
                           pageHistoryUrl.fold[List[FieldAttr]](Nil)(url => List("$push" -> Json.obj("pageHistory" -> url)))).toArray: _*),
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
          pageHistoryUrl.fold(Future.successful(Right(ProcessContext(sp.process, sp.answers, sp.labels, sp.flowStack, sp.stanzaPool, sp.urlToPageId, None)))){pageUrl =>
            val (backLink, historyUpdate) = backlinkAndHistory(pageUrl, previousPageByLink, sp.pageHistory)
            val processContext = ProcessContext(sp.process, sp.answers, sp.labels, sp.flowStack, sp.stanzaPool, sp.urlToPageId, backLink)
            historyUpdate.fold(Future.successful(Right(processContext)))(history =>
              savePageHistory(key, history).map {
                case Left(err) =>
                  logger.error(s"Unable to save backlink history, error = $err")
                  Right(processContext)
                case _ => Right(processContext)
              }
            )
          }
        }
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to retrieve process from session repo with _id=$key")
          Left(DatabaseError)
      }
  }

  def saveFormPageState(key: String, url: String, answer: String, labels: Labels): Future[RequestOutcome[Unit]] = {
    println(s"POOL: ${labels.updatedPool.toList.map(l => toFieldPair(s"labels.${l._1}", l._2))}")
    println(s"LABELS: ${labels.updatedLabels.values.map(l => toFieldPair(s"labels.${l.name}", l))}")
    findAndUpdate(
      Json.obj("_id" -> key),
      Json.obj(
        "$set" -> Json.obj(
          (List(
            toFieldPair(ttlExpiryFieldName, Json.obj(toFieldPair("$date", Instant.now().toEpochMilli))),
            toFieldPair("flowStack", labels.flowStack),
            toFieldPair(s"answers.$url", answer)) ++
            labels.updatedPool.toList.map(l => toFieldPair(s"stanzaPool.${l._1}", l._2)) ++
            labels.updatedLabels.values.map(l => toFieldPair(s"labels.${l.name}", l))).toArray: _*
        )
      )
    ).map { result =>
        result
          .result[DefaultSessionRepository.SessionProcess]
          .fold {
            logger.warn(
              s"Attempt to saveUserAnswerAndLabels using _id=$key returned no result, lastError ${result.lastError}, url: $url, answer: $answer"
            )
            Left(NotFoundError): RequestOutcome[Unit]
          }(_ => Right({}))
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to update question answers and labels within session repo with _id=$key, url: $url, answer: $answer")
          Left(DatabaseError)
      }
  }

  def savePageState(key: String, labels: Labels): Future[RequestOutcome[Unit]] =
      findAndUpdate(
        Json.obj("_id" -> key),
        Json.obj("$set" -> Json.obj(
          (labels.updatedPool.toList.map(l => toFieldPair(s"stanzaPool.${l._1}", l._2)) ++
           labels.updatedLabels.values.map(l => toFieldPair(s"labels.${l.name}", l))).toArray :+ toFieldPair("flowStack", labels.flowStack) : _*)
        )
      ).map { result =>
        result
          .result[DefaultSessionRepository.SessionProcess]
          .fold {
            logger.warn(
              s"Attempt to saveLabels using _id=$key returned no result, lastError ${result.lastError}"
            )
            Left(NotFoundError): RequestOutcome[Unit]
          }(_ => Right({}))
      }
      .recover {
        case lastError =>
          logger.error(s"Error $lastError while trying to update labels within session repo with _id=$key")
          Left(DatabaseError)
      }

  def set(key: String, process: Process, urlToPageId: Map[String, String]): Future[RequestOutcome[Unit]] = {
    val sessionDocument = Json.toJson(
      DefaultSessionRepository.SessionProcess(key, process.meta.id, process, Map(), Nil, Map(), urlToPageId, Map(), Nil, Instant.now)
    )
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

  private def backlinkAndHistory(pageUrl: String,
                                 previousPageByLink: Boolean,
                                 priorHistory: List[String]): (Option[String], Option[List[String]]) =
    priorHistory.reverse match {
      // Initial page
      case Nil => (None, None)
      // REFRESH: Rewrite pageHistory as current history without current page just added, Back link is x
      case x :: xs if x == pageUrl => (xs.headOption, Some(priorHistory))
      // BACK: pageHistory becomes y :: xs and backlink is head of xs
      case _ :: y :: xs if y == pageUrl && !previousPageByLink => (xs.headOption, Some((y :: xs).reverse))
      // FORWARD: Back link x, pageHistory intact
      case x :: _ => (Some(x), None)
    }

  private def toFieldPair[A](name: String, value: A)(implicit w: Writes[A]): FieldAttr = name -> Json.toJsFieldJsValueWrapper(value)

  private def savePageHistory(key: String, pageHistory: List[String]): Future[RequestOutcome[Unit]] =
    findAndUpdate(
      Json.obj("_id" -> key),
      Json.obj("$set" -> Json.obj(ttlExpiryFieldName -> Json.obj("$date" -> Instant.now().toEpochMilli), s"pageHistory" -> pageHistory))
    ).map { result =>
      result
        .result[DefaultSessionRepository.SessionProcess]
        .fold {
          logger.warn(
            s"Attempt to savePageHistory using _id=$key returned no result, lastError ${result.lastError}"
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
