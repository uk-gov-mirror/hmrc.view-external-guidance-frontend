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

import models.ocelot.{Phrase, Process}
import models.ocelot.stanzas.{ValueStanza, PageStanza, PageUrlValueName}

package object utils {

  def migrateValueStanzaProcess(p: Process): Process =
    p.copy(flow = p.flow.keys.map { k =>
      val s = p.flow(k) match {
        case v: ValueStanza => // Expects valid Process json
          PageStanza(v.values.find(_.label.equals(PageUrlValueName.toString)).get.value, v.next, v.stack)
        case s => s
      }
      (k, s)
    }.toMap)

  def rewriteWithWelsh(p: Process): Process = p.copy(phrases = p.phrases.map(p => Phrase(Vector(p.langs(0), s"Welsh, ${p.langs(0)}"))))
}
