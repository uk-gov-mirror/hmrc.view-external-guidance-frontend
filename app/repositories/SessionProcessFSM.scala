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

package repositories

import javax.inject.{Inject, Singleton}
import core.models.ocelot.FlowStage
import DefaultSessionRepository._


@Singleton
class SessionProcessFSM @Inject() {
  // Input
  // url ,incoming url
  // priorSp, prior SessionProcess corresponding to last url processed. Note. The db record will have the head of the page history updated
  //          to include incoming url and lowStack Nil. This update is not within priorSp. This is an optimisation, as the most common transition
  //          is forward in a process containing no Sequences (i.e. the flowStack will always be empty), this transition will result in
  //          no page history or flowstack updates given the update described has already taken place.
  // forceForward, true indicates that a url similar to head of prior history (looks like a BACK) should be treated as a forward movement
  //
  // Output
  // optional backlink to be displayed on page with incoming url
  // optional page history update
  // optional flowStack update

  // private def backlinkAndHistory(pageUrl: String,
  //                                flowStack: List[FlowStage],
  //                                previousPageByLink: Boolean,
  //                                priorHistory: List[PageHistory]): (Option[String], Option[List[PageHistory]], Option[List[FlowStage]]) =
  //   priorHistory.reverse match {
  //     // Initial page
  //     case Nil => (None, None, None)
  //     // REFRESH: Rewrite pageHistory as current history without current page just added, Back link is x
  //     case x :: xs if x.url == pageUrl => (xs.headOption.map(_.url), Some(priorHistory), None)
  //     // BACK: pageHistory becomes y :: xs and backlink is head of xs
  //     case _ :: y :: xs if y.url == pageUrl && !previousPageByLink => (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack))
  //     // FORWARD to start url: Back link x, rewrite pageHistory with current flowStack in head, set flowStack to that of initial page in history
  //     case x :: xs if priorHistory.headOption.fold(false)(h => h.url == pageUrl) =>
  //       (Some(x.url), Some((PageHistory(pageUrl, flowStack) :: x :: xs).reverse), priorHistory.headOption.map(_.flowStack))
  //     // FORWARD with a non-empty flowStack: Back link x, rewrite pageHistory with current flowStack in head
  //     case x :: xs if flowStack.nonEmpty => (Some(x.url), Some((PageHistory(pageUrl, flowStack) :: x :: xs).reverse), None)
  //     // FORWARD: Back link x, pageHistory intact
  //     case x :: _ => (Some(x.url), None, None)
  //   }

  def apply(url: String, priorSp: SessionProcess, forceForward: Boolean):  (Option[String], Option[List[PageHistory]], Option[List[FlowStage]]) = {
    println(s"**** FSM url $url, forceForward, $forceForward")
    println(s"**** FSM hist ${priorSp.pageHistory.reverse}")
    println(s"**** FSM flow ${priorSp.flowStack}")
    val newState = priorSp.pageHistory.reverse match {
      // Initial page
      case Nil => (None, None, None)
      // REFRESH: Rewrite pageHistory as current history without current page just added, Back link is x
      case x :: xs if x.url == url => (xs.headOption.map(_.url), Some(priorSp.pageHistory), None)
      // BACK: pageHistory becomes y :: xs and backlink is head of xs
      case _ :: y :: xs if y.url == url && !forceForward => (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack))
      // FORWARD to start url: Back link x, rewrite pageHistory with current flowStack in head, set flowStack to that of initial page in history
      case x :: xs if priorSp.pageHistory.headOption.fold(false)(h => h.url == url) =>
        (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), priorSp.pageHistory.headOption.map(_.flowStack))
      // FORWARD with a non-empty flowStack: Back link x, rewrite pageHistory with current flowStack in head
      case x :: xs if priorSp.flowStack.nonEmpty => (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None)
      // FORWARD: Back link x, pageHistory intact
      case x :: _ => (Some(x.url), None, None)
    }
    println(s"**** OUT backlink ${newState._1}")
    println(s"**** OUT hist ${newState._2.map(_.reverse)}")
    println(s"**** OUT flow ${newState._3}")
    newState
  }
}

      // case Nil => (None, None, None, None) // Initial Page
      // // REFRESH: Rewrite pageHistory as current history without current page just added, this is conditional on there being no change to the FlowStack
      // case x :: xs if x.url == url => (xs.headOption.map(_.url), Some(priorSp.pageHistory), None, None)

      // // BACK: pageHistory becomes y :: xs and backlink is head of xs, conditional on forward == false
      // case _ :: y :: xs if y.url == url && !forceForward =>
      //   val labelUpdate: Option[List[ScalarLabel]] = y.flowStack.reverse.headOption.collect{
      //     case f: Flow if f.labelValue.nonEmpty && f.labelValue.get.value.nonEmpty => List(ScalarLabel(f.labelValue.get.name, List(f.labelValue.get.value.get)))
      //   }
      //   println(s"*************** $labelUpdate")
      //   (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack), labelUpdate)

      // // FORWARD with a non-empty flowStack: Back link x, rewrite pageHistory with current flowStack in head
      // case x :: xs if priorSp.flowStack.nonEmpty => (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None, None)

      // // FORWARD: Back link x, pageHistory intact
      // case x :: _ => (Some(x.url), None, None, None)
