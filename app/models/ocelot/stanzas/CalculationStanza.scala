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

package models.ocelot.stanzas

import models.ocelot.{asCurrency, labelReference, labelReferences, Label, Labels}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._
import scala.annotation.tailrec

case class CalculationStanza(calcs: Seq[CalcOperation], override val next: Seq[String], stack: Boolean) extends Stanza {
  override val labels: List[Label] = calcs.map(op => Label(op.label)).toList
  override val labelRefs: List[String] = calcs.flatMap(op => labelReferences(op.left) ++ labelReferences(op.right)).toList
}

object CalculationStanza {

  implicit val calculationReads: Reads[CalculationStanza] =
    (
      (JsPath \ "calcs").read[Seq[CalcOperation]](minLength[Seq[CalcOperation]](1)) and
        (JsPath \ "next").read[Seq[String]](minLength[Seq[String]](1)) and
        (JsPath \ "stack").read[Boolean]
    )(CalculationStanza.apply _)

  implicit val calculationWrites: OWrites[CalculationStanza] =
    (
      (JsPath \ "calcs").write[Seq[CalcOperation]] and
        (JsPath \ "next").write[Seq[String]] and
        (JsPath \ "stack").write[Boolean]
      )(unlift(CalculationStanza.unapply))
}

sealed trait Operation {

  val left: String
  val right: String
  val label: String

  def eval(labels: Labels) : Labels

  def value(arg: String, labels: Labels): String = labelReference(arg).fold(arg){ref => labels.value(ref).getOrElse("")}

  def op(f: (BigDecimal, BigDecimal) => BigDecimal, labels: Labels) : Labels = {

    val x: String = value(left, labels)
    val y: String = value(right, labels)

    (asCurrency(x), asCurrency(y)) match {
      case (Some(bg1), Some(bg2)) => {

        val bg3 = f(bg1, bg2)

        labels.update(label, bg3.bigDecimal.toPlainString)

      }
      case _ => labels // Currently only support currency
    }

  }

}

case class Add(left: String, right: String, label: String) extends Operation {

  def eval(labels: Labels): Labels = op(_ + _, labels)

}

case class Subtract(left: String, right: String, label: String) extends Operation {

  def eval(labels: Labels): Labels = op(_ - _, labels)

}

case class Calculation(override val next: Seq[String], calcs: Seq[Operation]) extends Stanza with Evaluate {

  override val labels: List[Label] = calcs.map(op => Label(op.label)).toList
  override val labelRefs: List[String] = calcs.flatMap(op => labelReferences(op.left) ++ labelReferences(op.right)).toList

  @tailrec
  private def executeOperations(calcs: Seq[Operation], labels: Labels): Labels = {
    calcs match {
      case Nil => labels
      case c :: cs => executeOperations(cs, c.eval(labels))
    }
  }

  def eval(labels: Labels): (String, Labels) = {

    (next.last, executeOperations(calcs, labels))

  }

}

object Calculation {

  def apply(stanza: CalculationStanza): Calculation =

    Calculation(
      stanza.next,
      stanza.calcs.map{ c =>
        c.op match {
          case Addition => Add(c.left, c.right, c.label)
          case Subtraction => Subtract(c.left, c.right, c.label)
        }
      }
    )
}