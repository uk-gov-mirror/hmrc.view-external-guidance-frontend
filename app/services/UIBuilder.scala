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
import models.ocelot.Link
import models.ui._
//import scala.annotation.tailrec

object UIBuilder {

  val urlLinkRegex =   """\[link:(.*):([^:]*:[^:]*)\]""".r
  val stanzaLinkPattern = """\[link:(.+):(\d+)\]""".r
  val boldPattern = """\[bold:(.*)\]""".r

  def fromStanzaPage(pge: models.ocelot.Page): Page =
    Page(
      pge.url,
      pge.stanzas.flatMap{
        case Callout(typ,txt,_,_) if typ == Title  => Seq(H1(Text(txt.langs)))
        case Callout(typ,txt,_,_) if typ == SubTitle => Seq(H3(Text(txt.langs)))
        case Callout(typ,txt,_,_) if typ == Lede => Seq(Paragraph(Seq(Text(txt.langs)), true))
        case Instruction(txt,_,Some(Link(id,dest,_,window)),_) => Seq(Paragraph(Seq(HyperLink(dest, Text(txt.langs), window)), false))
        case Instruction(txt,_,lnk,_) => Seq(Paragraph(Seq(Text(txt.langs)), false))
        case ValueStanza(_,_,_) => List[UIComponent]()
        case EndStanza => List[UIComponent]()
      }.toSeq
    )

  def pages(stanzaPages: Seq[models.ocelot.Page]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p))).toMap
}