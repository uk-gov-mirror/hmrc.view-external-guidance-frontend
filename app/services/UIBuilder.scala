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
import models.ocelot.{Link, Phrase}
import models.ui._
import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object UIBuilder {

  def fromCallout(c: Callout): UIComponent =
    c.noteType match {
      case Title => H1(Text(c.text.langs))
      case SubTitle => H2(Text(c.text.langs))
      case Lede => Paragraph(Seq(Text(c.text.langs)), true)
      case Error => H3(Text(c.text.langs)) // TODO
    }

  def fromStanzaPage(pge: models.ocelot.Page): Page =
    Page(
      pge.url,
      pge.stanzas.flatMap{
        case c: Callout => Seq(fromCallout(c))

        case Instruction(txt,_,Some(Link(id,dest,_,window)),_) =>
          Seq(Paragraph(Seq(HyperLink(dest, Text(txt.langs), window)), false))
        case Instruction(txt,_,_,_) =>
          Seq(Paragraph(fromText(txt), false))

        case Question(_,_,_,_) => List[UIComponent]() // TODO
        case ValueStanza(_,_,_) => List[UIComponent]()
        case EndStanza => List[UIComponent]()
      }
    )

  def pages(stanzaPages: Seq[models.ocelot.Page]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p))).toMap


  def fromText(txt: Phrase): Seq[TextItem] = {

    def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) =
      (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)

    def matchesToLinks(enM: List[Match], cyM: List[Match]): List[HyperLink] =
      enM.zip(cyM).map{ t =>
        val( en, cy) = t
        HyperLink( en.group(2), Text(en.group(1), cy.group(1)), false)
      }

    def textToTexts(enT: List[String], cyT: List[String]): List[Text] =
      enT.zip(cyT).map{ t =>
        val (en, cy) = t
        Text(en, cy)
      }

    @tailrec
    def joinTextsAndLinks(txts: List[Text], links: List[HyperLink], acc: Seq[TextItem]): Seq[TextItem] =
      (txts, links) match {
        case (Nil, Nil) =>  acc
        case (t :: txs, l :: lxs ) => joinTextsAndLinks(txs, lxs, (acc :+t) :+ l)
        case (t, Nil) => acc ++ t
        case (Nil, _) => acc
      }

    val (enTexts, enMatches) = fromPattern(urlHttpLinkRegex, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(urlHttpLinkRegex, txt.langs(1))

    joinTextsAndLinks(textToTexts(enTexts, cyTexts),
                      matchesToLinks(enMatches, cyMatches),
                      Nil)
  }

  // TODO handle [] within label text????
  // TODO use urlLinkRegex to capture stand and http links in one match
  val urlHttpLinkRegex =   """\[link:(.*?):(http[s]?:[a-zA-Z0-9\/\.\-\?_\.=]+)\]""".r
  val urlStanzaLinkRegex =   """\[link:([^:]+):(\d+)\]""".r
  val urlLinkRegex = """\[link:([^:]+):(([htps]+:[a-zA-Z0-9\/\.\-]+))|(([^:]+):(\d+))\]""".r
  val boldPattern = """\[bold:(.*)\]""".r
}