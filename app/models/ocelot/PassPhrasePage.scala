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

package models.ocelot

// import models.ocelot.stanzas.Stanza
// import play.api.i18n.{MessagesApi, Lang}
import models.ocelot.stanzas.{Choice, EqualsTest, TextInput, PageStanza}

object PassPhrasePage {
  val PageId = "passpage"
  val InputId = "passinput"
  val ChoiceId = "passchoice"
  val keyedStanzas = Seq(
    KeyedStanza(PageId, PageStanza("/guard", Seq(InputId), false)),
    KeyedStanza(InputId, TextInput(Seq(ChoiceId),
                                   Phrase("Enter passphrase", "Welsh, Enter passphrase"),
                                   None,
                                   "_GuidancePassPhraseResponse",
                                   None,
                                   false)),
    KeyedStanza(ChoiceId, Choice(Seq(Process.StartStanzaId, PageId),
                                 Seq(EqualsTest("[label:_GuidancePassPhrase]","[label:_GuidancePassPhraseResponse]"))))
  )
  val page = Page("passpage", "/guard", keyedStanzas, Seq(Process.StartStanzaId))
}

