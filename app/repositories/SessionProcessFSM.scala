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
  //          to include incoming url and flowStack Nil, this update is not within priorSp. This is an optimisation, as the most common transition
  //          is forward in a process containing no Sequences (i.e. the flowStack will always be empty), this transition will result in
  //          no page history or flowstack updates given the update described has already taken place.
  // forceForward, true indicates that a url similar to head of prior history (looks like a BACK) should be treated as a forward movement

  // New state == (priorSp + new Pagehistory head) ++ pageHistory and flowStack output updates
  //
  // Output
  // optional backlink to be displayed on page with incoming url
  // optional page history update
  // optional flowStack update
  def apply(url: String, priorSp: SessionProcess, forceForward: Boolean):  (Option[String], Option[List[PageHistory]], Option[List[FlowStage]]) = {
    println(s"**** FSM url $url, forceForward, $forceForward")
    println(s"**** FSM hist ${priorSp.pageHistory.reverse}")
    println(s"**** FSM flow ${priorSp.flowStack}")
    //println(s"**** FSM flow ${priorSp}")
    val newState = priorSp.pageHistory.reverse match {
      // Initial page
      case Nil =>
        println(s"@@@@@ Nil")
        (None, None, None)
      // REFRESH: Rewrite pageHistory as current history without current page just added, Back link is x
      case x :: xs if x.url == url =>
        println(s"@@@@@ REFRESH")
        (xs.headOption.map(_.url), Some(priorSp.pageHistory), None)
      // BACK: pageHistory becomes y :: xs and backlink is head of xs (without flowstack update)
      case _ :: y :: xs if y.url == url && !forceForward && priorSp.flowStack == y.flowStack =>
        println(s"@@@@@ BACK (No flowStack update)")
        (xs.headOption.map(_.url), Some((y :: xs).reverse), None)
      // BACK: pageHistory becomes y :: xs and backlink is head of xs (without flowstack update)
      case _ :: y :: xs if y.url == url && !forceForward =>
        println(s"@@@@@ BACK")
        (xs.headOption.map(_.url), Some((y :: xs).reverse), Some(y.flowStack))
      // FORWARD with a non-empty flowStack: Back link x, rewrite pageHistory with current flowStack in head
      case x :: xs if priorSp.flowStack.nonEmpty =>
        println(s"@@@@@ FORCE FORWARD")
        (Some(x.url), Some((PageHistory(url, priorSp.flowStack) :: x :: xs).reverse), None)
      // FORWARD: Back link x, pageHistory intact
      case x :: _ =>
        println(s"@@@@@ FORWARD")
        (Some(x.url), None, None)
    }
    println(s"**** OUT backlink ${newState._1}")
    println(s"**** OUT hist ${newState._2.map(_.reverse)}")
    println(s"**** OUT flow ${newState._3.map(_.reverse)}")
    newState
  }
}

