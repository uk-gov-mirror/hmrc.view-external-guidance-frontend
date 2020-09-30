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

package services

import config.AppConfig
import javax.inject.{Inject, Singleton}
// import models.ui.{FormData, PageContext}
// import play.api.Logger
// import models.errors._
// import models.RequestOutcome
// import uk.gov.hmrc.http.HeaderCarrier
// import scala.concurrent.{ExecutionContext, Future}
import repositories.{ProcessContext, SessionRepository}
import models.ocelot.stanzas.VisualStanza
import models.ocelot.{Page, Labels}
import models.ocelot.KeyedStanza

@Singleton
class PageRenderer @Inject() (
    appConfig: AppConfig,
    sessionRepository: SessionRepository,
    uiBuilder: UIBuilder
) {

  def renderPage(page: Page, labels: Labels): (Seq[VisualStanza], Labels) = {
    // @tailrec
    // def evaluateStanzas(stanzas: Seq[KeyedStanza], visualStanzas: Seq[VisualStanza]): Seq[VisualStanza] =
    //   stanzas match {
    //     case Nil => visualStanzas
    //     case (e: Evaluate) :: xs => e.ev
    //   }

    (Nil, labels)
  }

}