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

package models.ui

import models.ocelot.asCurrency
import java.text.NumberFormat

trait Name {
  val name: String
}

sealed trait OutputFormat {
  def asString(value: Option[String]): String = value.getOrElse("")
}

case object Currency extends OutputFormat with Name {
  val name: String = "currency"
  override def asString(optValue: Option[String]): String =
    optValue.fold("")(value =>
      asCurrency(value) match {
        case Some(x) => NumberFormat.getCurrencyInstance().format(x)
        case None => ""
      }
    )
}
case object Number extends OutputFormat with Name {
  val name: String = "number"
}
case object Txt extends OutputFormat with Name {
  val name: String = "text"
}
case object Date extends OutputFormat with Name {
  val name: String = "date"
}

object OutputFormat {
  def apply(name: Option[String]): OutputFormat =
    name match {
      case Some(Currency.name) => Currency
      case Some(Number.name) => Number
      case Some(Txt.name) => Txt
      case Some(Date.name) => Date
      case _ => Txt
    }

}
