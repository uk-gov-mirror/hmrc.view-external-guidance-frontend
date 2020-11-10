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

trait UIComponent {
  val text: Text
}

//
// Monolingual
//
sealed trait TextItem {
  def isEmpty: Boolean
  def toWords: Seq[String]
  val bold: Boolean = false
}

case class Words(s: String, override val bold: Boolean = false) extends TextItem {
  def isEmpty: Boolean = s.isEmpty
  def toWords: Seq[String] = s.split(" +").toSeq
  override def toString: String = s
}

case class LabelRef(name: String, outputFormat: OutputFormat = Txt) extends TextItem {
  def isEmpty: Boolean = false
  def toWords: Seq[String] = name.split(" +").toSeq
  override def toString: String = s"[label:${name}:$outputFormat]"
}

case class Link(dest: String, text: String, window: Boolean = false, asButton: Boolean = false, hint:Option[String] = None) extends TextItem {
  override def toString: String = s"[${if(asButton) "button" else "link"}:$text:$dest:$window:$hint]"
  def isEmpty: Boolean = text.isEmpty
  def toWords: Seq[String] = text.split(" +").toSeq
}

//
// Bilingual
//
case class Text(english: Seq[TextItem], welsh: Seq[TextItem]) {
  def value(implicit lang: Lang): Seq[TextItem] = if (lang.code.equals("cy")) welsh else english
  def isEmpty(implicit lang: Lang): Boolean = if (lang.code.equals("cy")) welsh.isEmpty else english.isEmpty
  def toWords(implicit lang: Lang): Seq[String] = value(lang).flatMap(_.toWords)
  def asString(implicit lang: Lang): String = toWords.mkString(" ")
  override def toString: String = s"[${english.map(t => t.toString).mkString("")}:${welsh.map(t => t.toString).mkString("")}]"
  def +(other: Text): Text = Text(english ++ other.english, welsh ++ other.welsh)
  lazy val isBold: Boolean = english.length == 1 && english(0).bold
  lazy val isNumeric: Boolean = english.length == 1 && (english.head match {
    case l: LabelRef if l.outputFormat == Currency => true
    case _ => false
  })
}

object Text {
  def apply(english: TextItem, welsh: TextItem): Text = Text(Seq(english), Seq(welsh))
  def apply(english: String, welsh: String): Text = Text(Words(english), Words(welsh))
  def apply(phrase: Vector[String]): Text = Text(phrase(0), phrase(1))
  def apply(): Text = Text(Nil, Nil)

  def labelRef(name: String): Text = Text(LabelRef(name), LabelRef(name))
  def link(dest: String, phrase: Vector[String], window: Boolean = false, asButton: Boolean = false, hint: Option[Vector[String]] = None): Text =
    Text(Link(dest, phrase(0), window, asButton, hint.map(_(0))), Link(dest, phrase(1), window, asButton, hint.map(_(1))))
}
