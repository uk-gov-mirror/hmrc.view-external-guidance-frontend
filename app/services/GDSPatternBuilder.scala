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

import javax.inject.Singleton

@Singleton
class GDSPatternBuilder {

  // Heading Callout + RowGroup of 3 columns with column three link only => CyaGroup(title, rows: RowGroup): VisualStanza
  // Heading Callout (Optional) + RowGroup + First row all bold (Optional) => TableGroup(title: Option, columnHeaders: Option, rows: RowGroup): VisualStanza
  // Heading Callout with stack == true => Medium heading styling, large otherwise

  // transform Seq[VisualStanza] into (HeadingClass, Seq[VisualStanza])
}