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

import javax.inject.Singleton
import models.ocelot._
import models.ocelot.stanzas._
import models.ocelot.errors._
import play.api.Logger
import scala.annotation.tailrec

@Singleton
class PageBuilder extends ProcessPopulation {
  val logger: Logger = Logger(this.getClass)

  @tailrec
  private def collectStanzas(keys: Seq[String], ids: Seq[String], stanzas: Seq[Stanza], next: Seq[String])
                            (implicit process: Process): Either[GuidanceError, (Seq[String], Seq[Stanza], Seq[String])] =
    keys match {
      case Nil => Right((ids, stanzas, next))
      case key :: Nil if ids.contains(key) => Right((ids, stanzas, next))                                // Already encountered this stanzas within page
      case key :: xs if ids.contains(key) => collectStanzas(xs, ids, stanzas, next)                      // Already encountered but more paths to follow
      case key :: xs =>
        (stanza(key, process), xs ) match {
          case (Right(s: PageStanza), Nil) if ids.nonEmpty => Right((ids, stanzas, key +: next))         // End of page
          case (Right(s: PageStanza), _) if ids.nonEmpty => collectStanzas(xs, ids, stanzas, key +: next)// End of page but more paths to follow
          case (Right(s: PageStanza), _) => collectStanzas(xs ++ s.next, ids :+ key, stanzas :+ s, next) // Beginning of page
          case (Right(EndStanza), Nil) => Right((ids :+ key, stanzas :+ EndStanza, next))                // End of page
          case (Right(EndStanza), _) => collectStanzas(xs, ids :+ key, stanzas :+ EndStanza, next)       // End of page but more paths to follow

          case (Right(s: Stanza), _) => collectStanzas(xs ++ s.next, ids :+ key, stanzas :+ s, next)
          case (Left(err), _) => Left(err)
        }
    }

  def buildPage(key: String)(implicit process: Process): Either[GuidanceError, Page] = {

    collectStanzas(List(key), Nil, Nil, Nil) match {
      case Right((ids, stanzas, next)) =>
        val ks: Seq[KeyedStanza] = ids.zip(stanzas).map(t => KeyedStanza(t._1, t._2))
        ks.head.stanza match {
          case p: PageStanza if p.url.isEmpty || p.url.equals("/") => Left(PageUrlEmptyOrInvalid(ks.head.key))
          case p: PageStanza => Right(Page(ks.head.key, p.url, ks, next))
          case _ => Left(PageStanzaMissing(ks.head.key))
        }
      case Left(err) => Left(err)
    }
  }

  @tailrec
  private def duplicateUrlErrors(pages: Seq[Page], errors: List[GuidanceError]): List[GuidanceError] =
    pages match {
      case Nil => errors
      case x :: xs if xs.exists(_.url == x.url) => duplicateUrlErrors(xs, DuplicatePageUrl(x.id, x.url) :: errors)
      case x :: xs => duplicateUrlErrors(xs, errors)
    }

  // private def validateQuestionPage(p: Seq[Stanza]): List[GuidanceError] = {

  // }

  // @tailrec
  // private def checkForVisualStanzasAfterQuestions(pages: Seq[Page], errors: List[GuidanceError]): List[GuidanceError] =
  //   pages match {
  //     case Nil => errors
  //     case x :: xs if x.stanzas.exists( s => s match {
  //                     case q: Question => true
  //                     case _ => false
  //                   }
  //                  ) => checkForVisualStanzasAfterQuestions(xs, validateQuestionPage(x.stanzas) :: errors )
  //     case x :: xs => checkForVisualStanzasAfterQuestions(xs, errors)
  //   }

  def pagesWithValidation(process: Process, start: String = Process.StartStanzaId): Either[List[GuidanceError], Seq[Page]] =
    pages(process, start).fold[Either[List[GuidanceError], Seq[Page]]]( e => Left(e), pages => {
        duplicateUrlErrors(pages.reverse, Nil) match {
          case Nil => Right(pages)
          case duplicates => Left(duplicates)
        }
      }
    )

  def pages(process: Process, start: String = Process.StartStanzaId): Either[List[GuidanceError], Seq[Page]] = {
    @tailrec
    def pagesByKeys(keys: Seq[String], acc: Seq[Page]): Either[GuidanceError, Seq[Page]] =
      keys match {
        case Nil => Right(acc)
        case key :: xs if !acc.exists(_.id == key) =>
          buildPage(key)(process) match {
            case Right(page) =>
              pagesByKeys(page.next ++ xs ++ page.linked, acc :+ page)
            case Left(err) =>
              logger.error(s"Page building failed with error - $err")
              Left(err)
          }
        case _ :: xs => pagesByKeys(xs, acc)
      }

    pagesByKeys(List(start), Nil) match {
      case Left(err) => Left(List(err))
      case Right(pages) => Right(pages)
    }
  }

  def fromPageDetails[A](pages: Seq[Page])(f: (String, String, String) => A): List[A] =
    pages.toList.flatMap { page =>
      page.stanzas.collectFirst {
        case Callout(Title, text, _, _) =>
          f(page.id, page.url, text.langs(0))
        case q: Question =>
          f(page.id, page.url, hintRegex.replaceAllIn(q.text.langs(0), ""))
      }
    }
}
