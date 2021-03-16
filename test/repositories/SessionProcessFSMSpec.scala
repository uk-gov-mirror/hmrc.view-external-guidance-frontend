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

package repositories

import base.BaseSpec
import core.models.ocelot.{Process, ProcessJson, FlowStage}
import DefaultSessionRepository._
import java.time.Instant

class SessionProcessFSMSpec extends BaseSpec with ProcessJson {
  type BackLinkAndStateUpdate = (Option[String], Option[List[PageHistory]], Option[List[FlowStage]])
  trait Test {
    val sessionProcessSimpleHistory: SessionProcess =
      new SessionProcess(
        "id",
        "processId",
        validOnePageJson.as[Process],
        Map(),
        Nil,
        Map(),
        Map(),
        Map(),
        List(PageHistory("/start", Nil)),
        Instant.now
      )

    def verify(fsmOutput: BackLinkAndStateUpdate, bl: Option[String], ph: Option[List[PageHistory]], fs: Option[List[FlowStage]]): Unit = {
      fsmOutput._1 shouldBe bl
      fsmOutput._2 shouldBe ph
      fsmOutput._3 shouldBe fs
    }
  }

  val fsm = new SessionProcessFSM

  "SessionProcessFSM" must {
    "Return no backlink or updates for any url with no page history, forceForward false (Nil)" in new Test {
      verify(fsm("/start", SessionProcess("id", "processId", validOnePageJson.as[Process]), false),
             None,
             None,
             None)
    }

    "Return no backlink or updates for any url with no page history, forceForward true (Nil)" in new Test {
      verify(fsm("/start", SessionProcess("id", "processId", validOnePageJson.as[Process]), true),
             None,
             None,
             None)
    }

    "Return backlink and no updates for any url with single history and no flowStack, forceForward false (FORWARD)" in new Test {
      verify(fsm("/next", sessionProcessSimpleHistory, false),
             Some("/start"),
             None,
             None)
    }

    "Return backlink and no updates for any url with single history and no flowStack, forceForward true (FORWARD)" in new Test {
      verify(fsm("/next", sessionProcessSimpleHistory, true),
             Some("/start"),
             None,
             None)
    }

    "Return no backlink, + PageHistory update for repitition of the last url, with single history, no flowStack, forceForward false (REFRESH)" in new Test {
      verify(fsm("/start", sessionProcessSimpleHistory, false),
             None,
             Some(List(PageHistory("/start", Nil))),
             None)
    }

    "Return no backlink, PageHistory update for repitition of the last url, with single history, no flowStack, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/start", sessionProcessSimpleHistory, true),
             None,
             Some(List(PageHistory("/start", Nil))),
             None)
    }

    "Return backlink, + PageHistory update for repitition of the last url, with multiple history, no flowStack, forceForward false (REFRESH)" in new Test {
      verify(fsm("/next", sessionProcessSimpleHistory.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
             None)
    }

    "Return backlink, PageHistory update for repitition of the last url, with multiple history, no flowStack, forceForward  true (REFRESH)" in new Test {
      verify(fsm("/next", sessionProcessSimpleHistory.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
             Some("/start"),
             Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
             None)
    }

    "Return no backlink, PageHistory update with two element history and no flowStack, forceForward false (BACK)" in new Test {
        verify(fsm("/start", sessionProcessSimpleHistory.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), false),
               None,
               Some(List(PageHistory("/start", Nil))),
               None)
    }

    "Return backlink, PageHistory update with multiple element history and no flowStack, forceForward false (BACK)" in new Test {
        verify(fsm("/next", sessionProcessSimpleHistory.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil), PageHistory("/another", Nil))), false),
               Some("/start"),
               Some(List(PageHistory("/start", Nil), PageHistory("/next", Nil))),
               None)
    }

    "Return backlink, No updates with single history and no flowStack, forceForward true (FORCE FORWARD)" in new Test {
        verify(fsm("/start", sessionProcessSimpleHistory.copy(pageHistory = List(PageHistory("/start", Nil), PageHistory("/next", Nil))), true),
               Some("/next"),
               None,
               None)
    }
  }
}
