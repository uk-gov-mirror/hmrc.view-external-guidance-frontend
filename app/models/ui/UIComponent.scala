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

import play.api.i18n.Lang

trait UIComponent

sealed trait BilingualText {
  val english: String
  val welsh: String

  def value(implicit lang: Lang): String = if (lang.code.equals("cy")) welsh else english
  override def toString: String = english
}

case class TestableBilingualText(english: String, welsh: String) extends BilingualText

sealed trait TextItem {
  def isEmpty:Boolean
}

case class Text(english: String, welsh: String, bold: Boolean = false) extends BilingualText with TextItem {
  def isEmpty:Boolean = english.isEmpty
  override def toString: String = s"[$english:$welsh:$bold]"
}

object Text {
  def apply(phrase: Vector[String]): Text = Text(phrase(0), phrase(1))
}

case class HyperLink(dest: String, txt: Text, window: Boolean = false) extends TextItem {
  override def toString: String = s"[link:$txt:$dest:$window]"
  def isEmpty:Boolean = txt.isEmpty
}

case class PageLink(dest: String, txt: Text) extends TextItem {
  override def toString: String = s"[link:$txt:$dest]"
  def isEmpty:Boolean = txt.isEmpty
}
