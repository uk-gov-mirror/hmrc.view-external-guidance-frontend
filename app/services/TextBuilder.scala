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

import models.ocelot.Phrase
import models.ui._

import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object TextBuilder {

  private def placeholdersToItems(enM: List[Match], cyM: List[Match])(implicit urlMap: Map[String, String]): List[TextItem] =
    enM.zip(cyM).map{ t =>
      val( en, cy) = t
      // TODO Assume en and cy similar order of placeholders for now
      (Option(en.group(1)), Option(cy.group(1))) match {
        case (Some(enBold), Some(cyBold)) =>
          Text(enBold, cyBold, true)

        case (None, None) =>
          val enTxt = en.group(2)
          val enDest = en.group(3)
          val cyTxt = cy.group(2)
          //val cyDest = cy.group(3)
          // TODO also assume en and cy links are identical
          if (enDest.forall(_.isDigit)) {
            PageLink(urlMap(enDest), Text(enTxt, cyTxt))
          }
          else {
            HyperLink(enDest, Text(enTxt, cyTxt))
          }

        // if en and cy dont match reconstitute original as Text
        case (Some(enBold),None) => Text(enBold, cy.group(2), true)
        case (None, Some(cyBold)) => Text(en.group(2), cyBold, true)
      }
    }

  private def fromPattern(pattern: Regex, text: String): (List[String], List[Match]) =
    (pattern.split(text).toList, pattern.findAllMatchIn(text).toList)

  private def textToTexts(enT: List[String], cyT: List[String]): List[Text] =
    enT.zip(cyT).map(t => Text(t._1, t._2))

  @tailrec
  private def merge[A,B](txts: List[A], links: List[A], acc: Seq[A], isEmpty:A => Boolean): Seq[A] =
    (txts, links) match {
      case (Nil, Nil) =>  acc
      case (t :: txs, l :: lxs ) if isEmpty(t) => merge(txs, lxs, acc :+ l, isEmpty)
      case (t :: txs, l :: lxs ) => merge(txs, lxs, (acc :+t) :+ l, isEmpty)
      case (t, Nil) => acc ++ t
      case (Nil, l) => acc ++ l
    }

  def wordsToDisplayAsList(str: String): List[String] = {
    val isEmpty:String => Boolean = _.isEmpty

    val (txts, matches) = fromPattern(placeholdersPattern, str)
    val matchTexts = matches.map(m => Option(m.group(1)).getOrElse(m.group(2)))
    merge(txts, matchTexts, Nil, isEmpty).mkString(" ").split(" +").toList
  }

  def wordsToDisplay(str: String): String = wordsToDisplayAsList(str).mkString(" ")

  def fromPhrase(txt: Phrase)(implicit urlMap: Map[String, String]): Seq[TextItem] = {
    val isEmpty: TextItem => Boolean = _.isEmpty

    val (enTexts, enMatches) = fromPattern(placeholdersPattern, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(placeholdersPattern, txt.langs(1))

    merge(textToTexts(enTexts, cyTexts), placeholdersToItems(enMatches, cyMatches), Nil, isEmpty)
  }

  def answerTextWithOptionalHint(txt: Phrase):(Text, Option[Text]) = {
    val (enTexts, enMatches) = fromPattern(answerHintPattern, txt.langs(0))
    val (cyTexts, cyMatches) = fromPattern(answerHintPattern, txt.langs(1))
    val hint = enMatches.headOption.map(enM => Text(enM.group(1), cyMatches.headOption.map(cyM => cyM.group(1)).getOrElse("")))
    (Text(enTexts.head, cyTexts.head), hint)
  }

  def determineMatchedLeadingText( text1: String, text2: String ) : String = {

    val noOfMatchingWords: Int = matchedLeadingWordsToDisplayAsList( text1, text2 )

    val texts: List[String] = placeholdersPattern.split(text2).toList
    val matches: List[Match] = placeholdersPattern.findAllMatchIn(text2).toList

    val (wordsProcessed, outputTexts, outputMatches) = locateTextsAndMatchesContainingLeadingText(
      noOfMatchingWords,
      texts,
      matches,
      Nil,
      Nil,
      true,
      0
    )

    val leadingTextItems: List[String] = constructLeadingText( noOfMatchingWords, outputTexts, outputMatches, Nil, true, 0 )

    leadingTextItems.mkString
  }

  def matchedLeadingWordsToDisplayAsList( text1: String, text2: String ) : Int = {

    val text1WordsToDisplayAsList: List[String] = wordsToDisplayAsList( text1 )
    val text2WordsToDisplayAsList: List[String] = wordsToDisplayAsList( text2 )

    val matchedWords: List[String] = (text1WordsToDisplayAsList zip text2WordsToDisplayAsList).takeWhile(t => t._1 == t._2).map(_._1)

    matchedWords.size
  }

  /**
   * Method identifies the text and match components containing a specified number of words. These words
   * usually match those identifying the leading text of a bullet point list
   *
   * @param noOfWordsToMatch - The number of words to be located
   * @param inputTexts - The text components for the whole string being searched
   * @param inputMatches - The match components for the whole string being searched
   * @param outputTexts - The texts components containing the matched words
   * @param outputMatches - The match components containing the matched words
   * @param matchText - Boolean flag indicating whether text or match elements are being tested for the method call
   * @param wordsProcessed - The number of matched words identified by previous recursive calls to this method
   *
   * @return The method returns the text and match components that contain the matched words
   */
  @tailrec
  def locateTextsAndMatchesContainingLeadingText(noOfWordsToMatch: Int,
                                                 inputTexts: List[String],
                                                 inputMatches: List[Match],
                                                 outputTexts: Seq[String],
                                                 outputMatches: Seq[Match],
                                                 matchText: Boolean,
                                                 wordsProcessed: Int
                                                ): (Int, List[String], List[Match]) = {

    if (matchText) {
      if (inputTexts.isEmpty) {
        (wordsProcessed, outputTexts.toList, outputMatches.toList)
      } else {
        val text: String = inputTexts.head
        val noOfWords: Int = wordsInString(text)
        if (wordsProcessed + noOfWords < noOfWordsToMatch) {
          locateTextsAndMatchesContainingLeadingText(
            noOfWordsToMatch,
            inputTexts.drop(1),
            inputMatches,
            outputTexts :+ text,
            outputMatches,
            !matchText,
            wordsProcessed + noOfWords)
        } else {
          (wordsProcessed + noOfWords, (outputTexts :+ text).toList, outputMatches.toList)
        }
      }
    } else {
      if (inputMatches.isEmpty) {
        (wordsProcessed, outputTexts.toList, outputMatches.toList)
      } else {
        val text: String = getMatchText(inputMatches.head)
        val noOfWords: Int = wordsInString(text)
        if (wordsProcessed + noOfWords < noOfWordsToMatch) {
          locateTextsAndMatchesContainingLeadingText(
            noOfWordsToMatch,
            inputTexts,
            inputMatches.drop(1),
            outputTexts,
            outputMatches :+ inputMatches.head,
            !matchText,
            wordsProcessed + noOfWords)
        } else {
          (wordsProcessed + noOfWords, outputTexts.toList, (outputMatches :+ inputMatches.head).toList)
        }
      }
    }
  }

  /**
   * Method gathers the leading text from the text and match components containing the match words
   *
   * @param wordLimit - The number of words in the text to be reconstructed
   * @param texts - The text components containing some of the matched words
   * @param matches - The match components containing some of the matched words
   * @param items - The list of text items containing the matched words
   * @param matchText - Boolean flag indicating whether text or match elements are being included for the method call
   * @param wordsProcessed - The number of words gathered by previous recursive calls to the method
   *
   * @return - Returns a list of strings containing the leading text
   */
  @tailrec
  def constructLeadingText(wordLimit: Int,
                           texts: List[String],
                           matches: List[Match],
                           items: List[String],
                           matchText: Boolean, wordsProcessed: Int): List[String] = {

    if (matchText) {
      if (texts.size == 1 && matches.size == 0) {
        val noOfWordsToAdd: Int = wordLimit - wordsProcessed
        val leadingText: String = extractLeadingMatchedWords( noOfWordsToAdd, texts.head )
        items :+ leadingText
      } else {
        val text: String = texts.head
        val noOfWords: Int = wordsInString(text)
        constructLeadingText(wordLimit, texts.drop(1), matches, items :+ text, !matchText, wordsProcessed + noOfWords)
      }
    } else {
      if (matches.size == 1 && texts.size == 0) {
        items :+ matches.head.toString
      } else {
        val text: String = getMatchText(matches.head)
        val noOfWords: Int = wordsInString(text)
        constructLeadingText(wordLimit, texts, matches.drop(1), items :+ matches.head.toString(), !matchText, wordsProcessed + noOfWords)
      }
    }

  }

  /**
   * Method returns a substring of the input text comprising "noOfMatchedWords" words separated by white space
   *
   * @param noOfMatchedWords - The number of words to be included in the string returned by the method
   * @param text - The text to be sub-sampled
   *
   * @return - Returns a sub-sample of the input text
   */
  def extractLeadingMatchedWords( noOfMatchedWords: Int, text: String ) : String = {

    val matches: List[Match] = notSpaceRegex.findAllMatchIn( text ).toList

    if( noOfMatchedWords <= matches.size ) {
      text.substring( 0, matches( noOfMatchedWords - 1 ).end )
    } else {
      text
    }
  }

  def getMatchText(m: Match): String = {

    Option(m.group(1)) match {
      case Some(value) => m.group(1)
      case None => m.group(2)
    }

  }

  def wordsInString(text: String): Int = {

    if (text == "") {
      0
    }
    else {
      notSpaceRegex.findAllMatchIn( text ).toList.size
    }

  }

  val answerHintPattern: Regex = """\[hint:([^\]]+)\]""".r
  val placeholdersPattern: Regex = """\[bold:([^\]]+)\]|\[link:([^\]]+?):(\d+|https?:[a-zA-Z0-9\/\.\-\?_\.=&]+)\]""".r
  val notSpaceRegex: Regex = """([^' ']+)""".r
}