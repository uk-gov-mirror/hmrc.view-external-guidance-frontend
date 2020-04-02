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

import models.ocelot.stanzas.{Stanza, Instruction, InstructionGroup}
import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object BulletPointBuilder {

  val notSpaceRegex: Regex = """([^ ]+)""".r
  val matchLimit: Int = 3

  @tailrec
  def groupBulletPointInstructions(inputSeq: Seq[Stanza], acc: Seq[Stanza]): Seq[Stanza] =
    inputSeq match {
      case Nil => acc
      case x :: xs =>
        x match {
          case i: Instruction if i.stack =>
            val matchedInstructions: Seq[Instruction] = groupMatchedInstructions(xs, Seq(i))
            if (matchedInstructions.size > 1) {
              groupBulletPointInstructions(xs.drop(matchedInstructions.size - 1), acc :+ InstructionGroup(matchedInstructions))
            } else {
              groupBulletPointInstructions(xs, acc :+ matchedInstructions.head)
            }
          case s: Stanza => groupBulletPointInstructions(xs, acc :+ s)
        }
    }

  @tailrec
  private def groupMatchedInstructions(inputSeq: Seq[Stanza], acc: Seq[Instruction]): Seq[Instruction] =
    inputSeq match {
      case Nil => acc
      case x :: xs =>
        x match {
          case i: Instruction if BulletPointBuilder.matchInstructions(acc.last, i) => groupMatchedInstructions(xs, acc :+ i)
          case _ => acc
        }
    }

  def fragmentsToDisplayAsList(str: String): List[String] = {
    val isEmpty: String => Boolean = _.isEmpty

    val (txts, matches) = TextBuilder.placeholderTxtsAndMatches(str)
    val matchTexts = matches.map(m => Option(m.group(1)).getOrElse(m.group(2)))

    val mergedTexts: Seq[String] = TextBuilder.merge(txts, matchTexts, Nil, isEmpty)

    val filteredMergedTexts: Seq[String] = mergedTexts.filter(_ != "")

    filteredMergedTexts.toList
  }

  def determineMatchedLeadingText(text1: String, text2: String): String = {

    val (text1NoOfWordsToDisplay, text2NoOfWordsToDisplay, matchedWords) = matchInstructionText( text1, text2 )

    val (texts, matches) = TextBuilder.placeholderTxtsAndMatches(text2)

    val (wordsProcessed, outputTexts, outputMatches) = locateTextsAndMatchesContainingLeadingText(
      matchedWords.size,
      texts,
      matches,
      Nil,
      Nil,
      matchText = true,
      0
    )

    constructLeadingText(matchedWords.size, outputTexts, outputMatches, Nil, matchText = true, wordsProcessed = 0).mkString
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
  def locateTextsAndMatchesContainingLeadingText(
      noOfWordsToMatch: Int,
      inputTexts: List[String],
      inputMatches: List[Match],
      outputTexts: Seq[String],
      outputMatches: Seq[Match],
      matchText: Boolean,
      wordsProcessed: Int
  ): (Int, List[String], List[Match]) = {

    if (matchText) {
      if (inputTexts.isEmpty) {
        if (inputMatches.isEmpty) {
          (wordsProcessed, outputTexts.toList, outputMatches.toList)
        } else {
          locateTextsAndMatchesContainingLeadingText(noOfWordsToMatch, inputTexts, inputMatches, outputTexts, outputMatches, !matchText, wordsProcessed)
        }
      } else {
        val text: String = inputTexts.head
        val noOfWords: Int = wordsInString(text)
        if (wordsProcessed + noOfWords < noOfWordsToMatch) {
          locateTextsAndMatchesContainingLeadingText(noOfWordsToMatch, inputTexts.drop(1), inputMatches, outputTexts :+ text,
            outputMatches, !matchText, wordsProcessed + noOfWords)
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
          locateTextsAndMatchesContainingLeadingText(noOfWordsToMatch, inputTexts, inputMatches.drop(1), outputTexts,
            outputMatches :+ inputMatches.head,
            !matchText,
            wordsProcessed + noOfWords
          )
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
  def constructLeadingText(
      wordLimit: Int,
      texts: List[String],
      matches: List[Match],
      items: List[String],
      matchText: Boolean,
      wordsProcessed: Int
  ): List[String] = {

    if (matchText) {
      if (texts.isEmpty && matches.nonEmpty) {
        constructLeadingText(wordLimit, texts, matches, items, !matchText, wordsProcessed)
      } else if (texts.size == 1 && matches.isEmpty) {
        val noOfWordsToAdd: Int = wordLimit - wordsProcessed
        val leadingText: String = extractLeadingMatchedWords(noOfWordsToAdd, texts.head)
        items :+ leadingText
      } else {
        val text: String = texts.head
        val noOfWords: Int = wordsInString(text)
        constructLeadingText(wordLimit, texts.drop(1), matches, items :+ text, !matchText, wordsProcessed + noOfWords)
      }
    } else {
      if (matches.size == 1 && texts.isEmpty) {
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
  def extractLeadingMatchedWords(noOfMatchedWords: Int, text: String): String = {

    val matches: List[Match] = notSpaceRegex.findAllMatchIn(text).toList

    if (noOfMatchedWords <= matches.size) {
      text.substring(0, matches(noOfMatchedWords - 1).end)
    } else {
      text
    }
  }

  def matchInstructions(i1: Instruction, i2: Instruction): Boolean = {
    // Apply matching logic to English text as this is how Ocelot works currently
    val ( i1NoOfWordsToDisplay, i2NoOfWordsToDisplay, matchedWords) = matchInstructionText( i1.text.langs(0), i2.text.langs(0) )

    // Matching instructions must have matching leading text followed dissimilar trailing text
    matchedWords.size >= matchLimit && (matchedWords.size < i1NoOfWordsToDisplay) && (matchedWords.size < i2NoOfWordsToDisplay)
  }

  def matchInstructionText(text1: String, text2: String) : ( Int, Int, List[String]) = {

    // Break instruction text into fragments
    val text1FragmentsToDisplay: List[String] = fragmentsToDisplayAsList(text1)
    val text2FragmentsToDisplay: List[String] = fragmentsToDisplayAsList(text2)

    // Break fragments into a list of non-whitespace components
    val text1WordsToDisplay: List[String] = text1FragmentsToDisplay.flatMap(_.split(' '))
    val text2WordsToDisplay: List[String] = text2FragmentsToDisplay.flatMap(_.split(' '))

    val matchedTextItems: List[String] = (text1WordsToDisplay zip text2WordsToDisplay).takeWhile(t => t._1 == t._2).map(_._1).filter(_ != "")

    (text1WordsToDisplay.size, text2WordsToDisplay.size, matchedTextItems)
  }

  def getMatchText(m: Match): String = Option(m.group(1)).getOrElse(m.group(2))

  def wordsInString(text: String): Int = notSpaceRegex.findAllMatchIn(text).toList.size
}
