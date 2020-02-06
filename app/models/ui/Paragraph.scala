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

package models.ui

sealed trait ParagraphItem

case class ParagraphText(english: String, welsh: String) extends BilingualText with ParagraphItem
case class BoldParagraphText(english: String, welsh: String) extends BilingualText with ParagraphItem
case class ParagraphLink(val dest: String,
                         val txt: Text,
                         val window: Boolean = false,
                         val hidden: Option[Text] = None) extends Link with ParagraphItem
case class Paragraph(items: Seq[ParagraphItem]) extends UIComponent
