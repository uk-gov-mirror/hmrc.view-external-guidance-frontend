/*
 * Copyright 2021 HM Revenue & Customs
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

import models.ocelot.stanzas.{VisualStanza, Instruction, InstructionGroup}
import scala.util.matching.Regex
import Regex._
import scala.annotation.tailrec

object BulletPointBuilder {

  val notSpaceRegex: Regex = """([^ ]+)""".r
  val matchLimit: Int = 3

  @tailrec
  def groupBulletPointInstructions(acc: Seq[VisualStanza])(inputSeq: Seq[VisualStanza]): Seq[VisualStanza] =
    inputSeq match {
      case Nil => acc
      case x :: xs =>
        x match {
          case i: Instruction =>
            val matchedInstructions: Seq[Instruction] = groupMatchedInstructions(xs, Seq(i))
            if (matchedInstructions.size > 1) {
              groupBulletPointInstructions(acc :+ InstructionGroup(matchedInstructions))(xs.drop(matchedInstructions.size - 1))
            } else {
              groupBulletPointInstructions(acc :+ matchedInstructions.head)(xs)
            }
          case s: VisualStanza => groupBulletPointInstructions(acc :+ s)(xs)
        }
    }

  @tailrec
  private def groupMatchedInstructions(inputSeq: Seq[VisualStanza], acc: Seq[Instruction]): Seq[Instruction] =
    inputSeq match {
      case Nil => acc
      case x :: xs =>
        x match {
          case i: Instruction if(i.stack && BulletPointBuilder.matchInstructions(acc.last, i)) => groupMatchedInstructions(xs, acc :+ i)
          case _ => acc
        }
    }

  def determineMatchedLeadingText(instructionGroup: InstructionGroup, langIndex: Int): String = {

    val noOfMatchedWords = noOfMatchedLeadingWordsForInstructionGroup(instructionGroup, langIndex)

    val (texts, matches) = TextBuilder.placeholderTxtsAndMatches(instructionGroup.group.head.text.langs(langIndex))

    val (wordsProcessed, outputTexts, outputMatches) = locateTextsAndMatchesContainingLeadingText(
      noOfMatchedWords,
      texts,
      0,
      matches,
      0,
      Nil,
      Nil,
      0
    )

    constructLeadingText(noOfMatchedWords, outputTexts, 0, outputMatches, 0, Nil, wordsProcessed = 0).mkString
  }

  private def noOfMatchedLeadingWordsForInstructionGroup(instructionGroup: InstructionGroup, langIndex: Int): Int = {

    val firstInstruction = instructionGroup.group.head

    val remainingInstructions = instructionGroup.group.drop(1)

    val matchedWordsSeq = remainingInstructions.map { i =>
      matchInstructionText(firstInstruction.text.langs(langIndex), i.text.langs(langIndex))._3
    }

    matchedWordsSeq.map(_.size).min
  }

  /**
    * Method identifies the text and match components containing a specified number of words. These words
    * usually match those identifying the leading text of a bullet point list
    *
    * @param noOfWordsToMatch - The number of words to be located
    * @param inputTexts - The text components for the whole string being searched
    * @param textsProcessed - The number of text elements processed so far
    * @param inputMatches - The match components for the whole string being searched
    * @param matchesProcessed - The number of match elements processed so far
    * @param outputTexts - The texts components containing the matched words
    * @param outputMatches - The match components containing the matched words
    * @param wordsProcessed - The number of matched words identified by previous recursive calls to this method
    *
    * @return The method returns the text and match components that contain the matched words
    */
  def locateTextsAndMatchesContainingLeadingText(
      noOfWordsToMatch: Int,
      inputTexts: List[String],
      textsProcessed: Int,
      inputMatches: List[Match],
      matchesProcessed: Int,
      outputTexts: List[String],
      outputMatches: List[Match],
      wordsProcessed: Int
  ): (Int, List[String], List[Match]) = {

    if (textsProcessed == inputTexts.size) {
      if (matchesProcessed == inputMatches.size) {
        (wordsProcessed, outputTexts, outputMatches)
      } else {
        locateMatchesContainingLeadingText(
          noOfWordsToMatch,
          inputTexts,
          textsProcessed,
          inputMatches,
          matchesProcessed,
          outputTexts,
          outputMatches,
          wordsProcessed
        )
      }
    } else {
      val text: String = inputTexts(textsProcessed)
      val noOfWords: Int = wordsInText(text, textsProcessed, inputMatches, matchesProcessed)
      if (processNextMatch(noOfWordsToMatch, wordsProcessed, noOfWords, inputTexts, textsProcessed, inputMatches, matchesProcessed)) {
        locateMatchesContainingLeadingText(
          noOfWordsToMatch,
          inputTexts,
          textsProcessed + 1,
          inputMatches,
          matchesProcessed,
          outputTexts :+ text,
          outputMatches,
          wordsProcessed + noOfWords
        )
      } else {
        (wordsProcessed + noOfWords, (outputTexts :+ text), outputMatches)
      }
    }
  }

  def locateMatchesContainingLeadingText(
      noOfWordsToMatch: Int,
      inputTexts: List[String],
      textsProcessed: Int,
      inputMatches: List[Match],
      matchesProcessed: Int,
      outputTexts: List[String],
      outputMatches: List[Match],
      wordsProcessed: Int
  ): (Int, List[String], List[Match]) = {

    if (matchesProcessed == inputMatches.size) {
      (wordsProcessed, outputTexts, outputMatches)
    } else {
      val text: String = TextBuilder.placeholderMatchText(inputMatches(matchesProcessed))
      val noOfWords: Int = wordsInMatchText(text, inputTexts, textsProcessed)
      if (processNextText(noOfWordsToMatch, wordsProcessed, noOfWords, inputTexts, textsProcessed, text)) {
        locateTextsAndMatchesContainingLeadingText(
          noOfWordsToMatch,
          inputTexts,
          textsProcessed,
          inputMatches,
          matchesProcessed + 1,
          outputTexts,
          outputMatches :+ inputMatches(matchesProcessed),
          wordsProcessed + noOfWords
        )
      } else {
        (wordsProcessed + noOfWords, outputTexts, (outputMatches :+ inputMatches(matchesProcessed)))
      }
    }
  }

  /**
    * Method gathers the leading text from the text and match components containing the match words
    *
    * @param wordLimit - The number of words in the text to be reconstructed
    * @param texts - The text components containing some of the matched words
    * @param textsProcessed - Count of number of text elements previously processed
    * @param matches - The match components containing some of the matched words
    * @param matchesProcessed - Count of number of match elements previously processed
    * @param items - The list of text items containing the matched words
    * @param wordsProcessed - The number of words gathered by previous recursive calls to the method
    *
    * @return - Returns a list of strings containing the leading text
    */
  private def constructLeadingText(
      wordLimit: Int,
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int,
      items: List[String],
      wordsProcessed: Int
  ): List[String] = {

    if ((textsProcessed == texts.size) && (matchesProcessed < matches.size)) {
      constructLeadingTextFromMatches(wordLimit, texts, textsProcessed, matches, matchesProcessed, items, wordsProcessed)
    } else if ((texts.size - textsProcessed == 1) && (matches.size == matchesProcessed)) {
      val noOfWordsToAdd: Int = wordLimit - wordsProcessed
      val leadingText: String = extractLeadingMatchedWords(noOfWordsToAdd, texts, textsProcessed, matches, matchesProcessed)
      items :+ leadingText
    } else {
      val text: String = texts(textsProcessed)
      val noOfWords: Int = wordsInText(text, textsProcessed, matches, matchesProcessed)
      constructLeadingTextFromMatches(wordLimit, texts, textsProcessed + 1, matches, matchesProcessed, items :+ text, wordsProcessed + noOfWords)
    }

  }

  private def constructLeadingTextFromMatches(
      wordLimit: Int,
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int,
      items: List[String],
      wordsProcessed: Int
  ): List[String] = {

    if ((matches.size - matchesProcessed == 1) && (texts.size == textsProcessed)) {
      items :+ matches(matchesProcessed).toString
    } else {
      val text: String = TextBuilder.placeholderMatchText(matches(matchesProcessed))
      val noOfWords: Int = wordsInMatchText(text, texts, textsProcessed)
      constructLeadingText(
        wordLimit,
        texts,
        textsProcessed,
        matches,
        matchesProcessed + 1,
        items :+ matches(matchesProcessed).toString(),
        wordsProcessed + noOfWords
      )
    }

  }

  /**
    * Method returns a substring of the input text comprising "noOfMatchedWords" words separated by white space
    *
    * @param noOfMatchedWords - The number of words to be included in the string returned by the method
    * @param texts - The text components containing some of the matched words
    * @param textsProcessed - Count of number of text elements previously processed
    * @param matches - The match components containing some of the matched words
    * @param matchesProcessed - Count of number of match elements previously processed
    *
    * @return - Returns a sub-sample of the final input text
    */
  private def extractLeadingMatchedWords(noOfMatchedWords: Int, texts: List[String], textsProcessed: Int, matches: List[Match], matchesProcessed: Int): String = {

    val textElements: List[Match] = notSpaceRegex.findAllMatchIn(texts(textsProcessed)).toList

    matches match {
      case Nil => {
        noOfMatchedWords match {
          case x if x <= textElements.size => texts(textsProcessed).substring(0, textElements(noOfMatchedWords - 1).end)
          case _ => texts(textsProcessed)
        }
      }
      case _ => {
        noOfMatchedWords match {
          case 0 => {
            // A value of zero for the number of matched words indicates single word in text immediately
            // following a bold or link annotation
            texts(textsProcessed).substring(0, textElements.head.end)
          }
          case _ => {
            getUpdatedLeadingMatchedWords(noOfMatchedWords, textElements, texts, textsProcessed, matches, matchesProcessed)
          }
        }
      }
    }
  }

  private def getUpdatedLeadingMatchedWords(
      noOfMatchedWords: Int,
      textElements: List[Match],
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int
  ): String = {

    val trailingText: Boolean = textTrailingMatchText(textsProcessed, texts, TextBuilder.placeholderMatchText(matches(matchesProcessed - 1)))

    val updatedNoOfWords: Int = updateNoOfMatchedWords(trailingText, noOfMatchedWords)

    updatedNoOfWords match {
      case x if x <= textElements.size => texts(textsProcessed).substring(0, textElements(updatedNoOfWords - 1).end)
      case _ => texts(textsProcessed)
    }
  }

  def matchInstructions(i1: Instruction, i2: Instruction): Boolean = {
    // Apply matching logic to English text as this is how Ocelot works currently
    val (i1NoOfWordsToDisplay, i2NoOfWordsToDisplay, matchedWords) = matchInstructionText(i1.text.langs(0), i2.text.langs(0))

    // Matching instructions must have matching leading text followed dissimilar trailing text
    matchedWords.size >= matchLimit && (matchedWords.size < i1NoOfWordsToDisplay) && (matchedWords.size < i2NoOfWordsToDisplay)
  }

  private def matchInstructionText(text1: String, text2: String): (Int, Int, Seq[String]) = {

    // Break instruction text into fragments
    val text1FragmentsToDisplay: Seq[String] = TextBuilder.flattenPlaceholders(text1)
    val text2FragmentsToDisplay: Seq[String] = TextBuilder.flattenPlaceholders(text2)

    // Break fragments into a list of non-whitespace components
    val text1WordsToDisplay: Seq[String] = text1FragmentsToDisplay.mkString.split(' ')
    val text2WordsToDisplay: Seq[String] = text2FragmentsToDisplay.mkString.split(' ')

    val matchedTextItems: Seq[String] = (text1WordsToDisplay zip text2WordsToDisplay).takeWhile(t => t._1 == t._2).map(_._1).filter(_ != "")

    (text1WordsToDisplay.size, text2WordsToDisplay.size, matchedTextItems)
  }

  private def wordsInString(text: String): Int = notSpaceRegex.findAllMatchIn(text).toList.size

  private def wordsInText(text: String, textsProcessed: Int, matches: List[Match], matchesProcessed: Int): Int = {

    val wordsInText: Int = wordsInString(text)

    if (textsProcessed == 0) {
      wordsInText
    } else {
      // For text following after placeholder check if there is a gap between the placeholder text and the following text
      if (!gapBetweenTextElements(TextBuilder.placeholderMatchText(matches(matchesProcessed - 1)), text)) {
        wordsInText - 1
      } else {
        wordsInText
      }
    }
  }

  private def wordsInMatchText(matchText: String, texts: List[String], textsProcessed: Int): Int = {

    val wordsInMatchText: Int = wordsInString(matchText)

    if (textsProcessed == 0) {
      wordsInMatchText
    } else {
      if (texts(textsProcessed - 1).isEmpty) {
        wordsInMatchText
      } else {
        if (!gapBetweenTextElements(texts(textsProcessed - 1), matchText)) {
          wordsInMatchText - 1
        } else {
          wordsInMatchText
        }
      }
    }
  }

  private def processNextMatch(
      noOfWordsToMatch: Int,
      wordsProcessed: Int,
      noOfWordsInCurrentText: Int,
      texts: List[String],
      textsProcessed: Int,
      matches: List[Match],
      matchesProcessed: Int
  ): Boolean = {

    val notAllWordsProcessed = wordsProcessed + noOfWordsInCurrentText < noOfWordsToMatch

    val textLeadsNextMatch: Boolean = (wordsProcessed + noOfWordsInCurrentText == noOfWordsToMatch) &&
      (matchesProcessed < matches.size) &&
      textLeadingMatchText(textsProcessed, texts, matches(matchesProcessed))

    notAllWordsProcessed || textLeadsNextMatch
  }

  private def processNextText(
      noOfWordsToMatch: Int,
      wordsProcessed: Int,
      noOfWordsInCurrentText: Int,
      texts: List[String],
      textsProcessed: Int,
      matchText: String
  ): Boolean = {

    val notAllWordsProcessed: Boolean = wordsProcessed + noOfWordsInCurrentText < noOfWordsToMatch

    val textFollowingPreviousMatch: Boolean = (wordsProcessed + noOfWordsInCurrentText == noOfWordsToMatch) &&
      textTrailingMatchText(textsProcessed, texts, matchText)

    notAllWordsProcessed || textFollowingPreviousMatch
  }

  private def textLeadingMatchText(textsProcessed: Int, texts: List[String], m: Match): Boolean = {

    if (gapBetweenTextElements(texts(textsProcessed), TextBuilder.placeholderMatchText(m))) false else true
  }

  private def textTrailingMatchText(textsProcessed: Int, texts: List[String], matchText: String): Boolean = {
    if (texts.size - 1 >= textsProcessed) {
      if (gapBetweenTextElements(matchText, texts(textsProcessed))) false else true
    } else {
      false
    }
  }

  private def updateNoOfMatchedWords(trailingText: Boolean, noOfMatchedWords: Int): Int = {

    if (trailingText) noOfMatchedWords + 1 else noOfMatchedWords

  }

  private def gapBetweenTextElements(text1: String, text2: String): Boolean = text1.endsWith(" ") || text2.startsWith(" ")
}
