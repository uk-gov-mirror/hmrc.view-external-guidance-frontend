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

package controllers.actions

import play.api.http.Status
import play.api.mvc._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.http.HeaderCarrier
import scala.concurrent.{ExecutionContext, Future}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

class SessionIdActionSpec extends base.ViewSpec  with GuiceOneAppPerSuite {

  case class Harness(action: SessionIdAction) {
    def onPageLoad(): Action[AnyContent] = action { _ => Results.Ok }
  }

  trait Test {

    val path: String = "/path"
    val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", path)

    implicit val hc: HeaderCarrier = HeaderCarrier()
    implicit val ec: ExecutionContext = app.injector.instanceOf[ExecutionContext]
    val sessionId = s"session-${java.util.UUID.randomUUID.toString}"
    val egSessionId = s"session-${java.util.UUID.randomUUID.toString}"
    lazy val sessionIdAction = new SessionIdActionImpl(app.injector.instanceOf[BodyParsers.Default])

    lazy val target = Harness(sessionIdAction)
  }

  "SessionIdAction" should {

    "Pass on requests containing only sessionid" in new Test {

      val result: Future[Result] = target.onPageLoad()(fakeRequest)

      status(result) mustBe Status.OK
    }

    "Convert EG_NEW_SESSIONID into a sessionId" in new Test {

      override val fakeRequest: FakeRequest[AnyContentAsEmpty.type] = FakeRequest("GET", path).withSession(sessionIdAction.EgNewSessionIdName -> egSessionId)
      val result: Result = await(target.onPageLoad()(fakeRequest))

      println(result)
    }

    "When included in the request, ensure it is not included in the Result" in new Test {

      val result: Future[Result] = target.onPageLoad()(fakeRequest)

      //status(result) mustBe UNAUTHORIZED
    }

  }

}
