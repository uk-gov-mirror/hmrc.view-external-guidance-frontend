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


import models.ocelot.stanzas._
import models.ui._
import scala.annotation.tailrec

object UIBuilder {

  val urlLinkRegex =   """\[link:(.*):([^:]*:[^:]*)\]""".r
  val stanzaLinkPattern = """\[link:(.+):(\d+)\]""".r
  val boldPattern = """\[bold:(.*)\]""".r

  def fromStanzaPage(pge: models.ocelot.Page): Either[UIError, Page] = {

    @tailrec
    def componentsFromStanzas(stanzas: Seq[Stanza], acc: Seq[UIComponent]): Either[UIError, Seq[UIComponent]] =
      stanzas match {
        case Nil => Right(acc)
        case Callout(typ,txt,_,_) :: xs if typ == Title  => componentsFromStanzas( xs, H1(Text(txt.langs)) +: acc )
        case Callout(typ,txt,_,_) :: xs if typ == SubTitle => componentsFromStanzas( xs, H3(Text(txt.langs)) +: acc )
        case Callout(typ,txt,_,_) :: xs if typ == Lede => componentsFromStanzas( xs, Paragraph(Seq(Text(txt.langs)), true) +: acc )
        case Instruction(txt,_,lnk,_) :: xs if lnk.isEmpty => componentsFromStanzas( xs, Paragraph(Seq(Text(txt.langs)), true) +: acc )
        case Instruction(txt,_,lnk,_) :: xs => componentsFromStanzas( xs, Paragraph(Seq(Text(txt.langs)), true) +: acc )
        case ValueStanza(_,_,_) :: xs => componentsFromStanzas( xs, acc )
        case EndStanza :: xs => componentsFromStanzas( xs, acc )
        case u :: xs => componentsFromStanzas( xs, acc )
      }

    componentsFromStanzas(pge.stanzas.values.toList, Nil).fold(
      err => Left(err),
      components => Right(Page(pge.url, components))
    )
  }

  def pages(stanzaPages: Seq[models.ocelot.Page]): Either[UIError, Map[String, Page]] = {

    @tailrec
    def pages(stanzaPages: Seq[models.ocelot.Page], acc: Seq[Page]): Either[UIError, Seq[Page]] =
      stanzaPages match {
        case Nil => Right(acc)
        case x :: xs => fromStanzaPage(x) match {
                          case Left(err) => Left(err)
                          case Right(page) => pages(xs, page +: acc)
                        }
      }

    pages(stanzaPages, Nil).fold(
      err => Left(err),
      pages => Right(pages.map( p => (p.urlPath, p)).toMap)
    )
  }

}