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

package models.ocelot.stanzas

case class NoteGroup (override val next: Seq[String], group: Seq[NoteCallout], stack: Boolean) extends VisualStanza with Populated

object NoteGroup {
  def apply(group: Seq[NoteCallout]): NoteGroup =
    group match {
      case Nil => NoteGroup(Seq.empty, Seq.empty, false)
      case _ => NoteGroup(group.last.next, group, group.head.stack)
    }
}
