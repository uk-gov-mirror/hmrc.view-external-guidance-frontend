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

package models.ocelot

import base.{BaseSpec, ProcessJson}
import play.api.libs.json._

import models.ocelot.stanzas.Stanza

class ProcesSpec extends BaseSpec with ProcessJson {

  "Process" must {

    "Deserialise from prototype json" in {

      val meta:Meta = Json.parse(prototypeMetaSection).as[Meta]
      val flow:Map[String, Stanza] = Json.parse(prototypeFlowSection).as[Map[String, Stanza]]
      val phrases:Vector[Phrase] = Json.parse(prototypePhrasesSection).as[Vector[Phrase]]
      val links: Vector[Link] = Json.parse(prototypeLinksSection).as[Vector[Link]]

      val process:Process = prototypeJson.as[Process]

      process.meta mustBe meta
      process.flow mustBe flow
      process.phrases mustBe phrases
      process.links mustBe links

    }

    missingJsObjectAttrTests[Process](prototypeJson, List("howto", "contacts"))

    incorrectPropertyTypeJsObjectAttrTests[Process](prototypeJson, List("howto", "contacts"))
  }
}
