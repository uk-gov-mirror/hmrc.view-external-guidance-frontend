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

package connectors.httpParsers

import models.errors.{InternalServerError, InvalidProcessError}
import models.RequestOutcome
import models.ocelot._
import play.api.Logger
import play.api.http.Status._
import uk.gov.hmrc.http.HttpReads

object GetScratchProcessHttpParser extends HttpParser {

  val logger: Logger = Logger(GetScratchProcessHttpParser.getClass)

  implicit val getScratchProcessHttpReads: HttpReads[RequestOutcome[Process]] = {
    case (_, _, response) if response.status == OK => response.validateJson[Process] match {
        case Some(result) => Right(result)
        case None =>
          logger.error("Unable to parse successful response when requesting process")
          Left(InternalServerError)
      }
    case (_, _, response) if response.status == NOT_FOUND =>
      logger.info("Not Found response received when requesting process")
      Left(InvalidProcessError)
    case (_, _, response) if response.status == BAD_REQUEST =>
      logger.info("Bad Request response received when requesting process")
      Left(InvalidProcessError)
    case unknown =>
      logger.info(s"${unknown} response received when requesting process")
      Left(InternalServerError)
  }

}
