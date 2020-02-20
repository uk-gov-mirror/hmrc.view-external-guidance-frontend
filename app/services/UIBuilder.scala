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
      pge.stanzas.foldLeft(Seq[UIComponent]()){(acc, el) =>
        el match {
          case c: Callout => acc ++ Seq(fromCallout(c))

          case Instruction(txt,_,Some(Link(id,dest,_,window)),_) =>
            acc ++ Seq(Paragraph(Seq(HyperLink(dest, Text(txt.langs), window)), false))
          case Instruction(txt,_,_,_) =>
            acc ++ Seq(Paragraph(fromText(txt), false))

          case Question(_,_,_,_) => acc // TODO
          case ValueStanza(_,_,_) => acc
          case EndStanza => acc
        }
      }
    )

  def pages(stanzaPages: Seq[models.ocelot.Page]): Map[String, Page] =
    stanzaPages.map(p => (p.url, fromStanzaPage(p))).toMap


  private def placeholdersToItems(enM: List[Match], cyM: List[Match]): List[TextItem] = {
    def isInteger(str: String): Boolean = str.forall(_.isDigit)

    def createLink(en: String, cy: String): TextItem = {
      val (enTxt, enLink) = linkRegex.findFirstMatchIn(en)
                                   .map(m => (m.group(1), m.group(2)))
                                   .getOrElse(("",""))
      val (cyTxt, cyLink) = linkRegex.findFirstMatchIn(cy)
                                   .map(m => (m.group(1), m.group(2)))
                                   .getOrElse(("",""))
      // TODO Assume en and cy similar order for now
      // TODO also assume en and cy links are identical
      if (isInteger(enLink)) {
        PageLink(enLink, Text(enTxt, cyTxt))
      }
      else {
        HyperLink(enLink, Text(enTxt, cyTxt))
      }
    }

    def createBoldText(en: String, cy: String): TextItem = Text(en, cy, true)

    enM.zip(cyM).map{ t =>
      val( en, cy) = t
      (en.group(1), cy.group(1)) match {
        case ("link", "link") => createLink(en.group(2), cy.group(2))
        case ("bold", "bold") => createBoldText(en.group(2), cy.group(2))
        case (_,_) => createBoldText(en.group(2), cy.group(2))
      }
    }
  }

  private def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) ={
    val texts = pattern.split(text).toList
    val matches = pattern.findAllMatchIn(text).toList
    (texts, matches)
  }

  private def textToTexts(enT: List[String], cyT: List[String]): List[Text] = enT.zip(cyT).map(t => Text(t._1, t._2))

  @tailrec
  private def mergeTextItems(txts: List[TextItem], links: List[TextItem], acc: Seq[TextItem]): Seq[TextItem] =
    (txts, links) match {
      case (Nil, Nil) =>  acc
      case (t :: txs, l :: lxs ) if t.isEmpty => mergeTextItems(txs, lxs, acc :+ l)
      case (t :: txs, l :: lxs ) => mergeTextItems(txs, lxs, (acc :+t) :+ l)
      case (t, Nil) => acc ++ t
      case (Nil, _) => acc
    }

  def fromText(txt: Phrase): Seq[TextItem] = {

    val (enTexts, enMatches) = fromPattern(placeholderRegex, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(placeholderRegex, txt.langs(1))

    val txtItems = textToTexts(enTexts, cyTexts)
    val placeholderItems = placeholdersToItems(enMatches, cyMatches)

    mergeTextItems(txtItems, placeholderItems, Nil)
  }

  //val urlHttpLinkRegex =   """\[link:(.*?):(http[s]?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
  //val urlStanzaLinkRegex =   """\[link:([^:]+):(\d+)\]""".r
  //val urlLinkRegex = """\[link:(.+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
  val placeholderRegex = """\[(link|bold):(.+?)\]""".r
  val linkRegex = """(.+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)""".r
  //val boldPattern = """\[bold:(.*)\]""".r
}