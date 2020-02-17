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
import scala.util.matching.Regex
import Regex._
//import scala.annotation.tailrec

object UIBuilder {

  // TODO handle [] within label text????
  val urlHttpLinkRegex =   """\[link:([^:]+):([htps]+:[a-zA-Z0-9\/\.\-]+)\]""".r
  val urlStanzaLinkRegex =   """\[link:([^:]+):(\d+)\]""".r
  val urlLinkRegex = """\[link:([^:]+):(([htps]+:[a-zA-Z0-9\/\.\-]+))|(([^:]+):(\d+))\]""".r
  val stanzaLinkPattern = """\[link:(.+):(\d+)\]""".r
  val boldPattern = """\[bold:(.*)\]""".r

  def fromCallout(c: Callout): UIComponent =
    c.noteType match {
      case Title => H1(Text(c.text.langs))
      case SubTitle => H3(Text(c.text.langs))
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
        case Instruction(txt,_,lnk,_) =>
          Seq(Paragraph(Seq(Text(txt.langs)), false))

        case Question(_,_,_,_) => List[UIComponent]() // TODO
        case ValueStanza(_,_,_) => List[UIComponent]()
        case EndStanza => List[UIComponent]()
      }
    )

  def pages(stanzaPages: Seq[models.ocelot.Page]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p))).toMap


  def textToTextItems(txt: Text): Seq[TextItem] = {

    def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) = {
      val texts = pattern.split(text).toList
      val matches = pattern.findAllMatchIn(text)
      (texts.toList, matches.toList)
    }

    def matchesToLInks(enM: List[Match], cyM: List[Match]): List[HyperLink] =
      enM.zip(cyM).map{ t =>
        val( en, cy) = t
        HyperLink( en.group(2), Text(en.group(1), cy.group(1)), false)
      }

    def textToTexts(enT: List[String], cyT: List[String]): List[Text] =
      enT.zip(cyT).map{ t =>
        val (en, cy) = t
        Text(en, cy)
      }

    // TODO
    def joinTextsAndLinks(txts: List[Text], links: List[HyperLink], acc: Seq[TextItem]): Seq[TextItem] =
      (txts, links) match {
        case (Nil, Nil) =>  acc
        case (t :: xs, Nil) => t +: acc
        case (Nil, _) => acc
        case (t :: txs, l :: lxs ) => joinTextsAndLinks(txs, lxs, t +: (l +: acc))
      }

    val (enTexts, enMatches) = fromPattern(urlHttpLinkRegex, txt.english)
    val (cyTexts, cyMatches) = fromPattern(urlHttpLinkRegex, txt.english)

    joinTextsAndLinks(textToTexts(enTexts, cyTexts),
                      matchesToLInks(enMatches, cyMatches),
                      Nil)
  }
}