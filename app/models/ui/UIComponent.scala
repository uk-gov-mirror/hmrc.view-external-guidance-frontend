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

//
// Monolingual
//
sealed trait TextItem {
  def isEmpty: Boolean
  def toWords: Seq[String]
}

case class Words(s: String, bold: Boolean = false) extends TextItem {
  def isEmpty: Boolean = s.isEmpty
  def toWords: Seq[String] = s.split(" +").toSeq
  override def toString: String = s
}

case class Link(dest: String, txt: String, window: Boolean = false) extends TextItem {
  override def toString: String = s"[link:$txt:$dest:$window]"
  def isEmpty: Boolean = txt.isEmpty
  def toWords: Seq[String] = txt.split(" +").toSeq
}

//
// Bilingual
//
case class Text(english: Seq[TextItem], welsh: Seq[TextItem]) {
  def value(implicit lang: Lang): Seq[TextItem] = if (lang.code.equals("cy")) welsh else english
  def isEmpty(implicit lang: Lang): Boolean = if (lang.code.equals("cy")) welsh.isEmpty else english.isEmpty
  def toWords(implicit lang: Lang): Seq[String] = value(lang).flatMap(_.toWords)
  override def toString: String = s"[${english.map(t => t.toString).mkString("")}:${welsh.map(t => t.toString).mkString("")}]"
  def +(other: Text): Text = Text(english ++ other.english, welsh ++ other.welsh)
}

object Text {
  def apply(phrase: Vector[String]): Text = Text(Words(phrase(0)), Words(phrase(1)))
  def apply(english: String, welsh: String): Text = Text(Seq(Words(english)), Seq(Words(welsh)))
  def apply(english: TextItem, welsh: TextItem): Text = Text(Seq(english), Seq(welsh))
  def apply(): Text = Text(Nil, Nil)
}
