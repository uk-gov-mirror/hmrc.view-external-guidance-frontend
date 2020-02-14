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

  def fromStanzaPage(pge: models.ocelot.Page): Either[UIError, Page] = {

    @tailrec
    def componentsFromStanzas(stanzas: Seq[Stanza], acc: Seq[UIComponent]): Either[UIError, Seq[UIComponent]] =
      stanzas match {
        case Nil => Right(acc)
        // case c: Callout :: xs =>
        // case q: Question :: xs =>
        // case i: Instruction :: xs =>
        // case v: ValueStanza :: xs =>
        case EndStanza :: xs => componentsFromStanzas( xs, acc )
        case u :: xs => Left(UnhandledStanza(u))
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