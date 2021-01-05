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

package models.ui

import models.ocelot.{asCurrency, asDate}
import java.util.Locale.UK
import java.math.RoundingMode
import java.text.NumberFormat
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatter.ofPattern

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
        case Some(x) => NumberFormat.getCurrencyInstance(UK).format(x)
        case None => value
      }
    )
}

case object CurrencyPoundsOnly extends OutputFormat with Name {
  val name: String = "currencyPoundsOnly"
  override def asString(optValue: Option[String]): String =
    optValue.fold("")(value =>
      // Extract as simple number, then format as pounds only
      asCurrency(value) match {
        case Some(x) =>
          val formatter = NumberFormat.getCurrencyInstance(UK)
          formatter.setMaximumFractionDigits(0)
          formatter.setRoundingMode(RoundingMode.DOWN)
          formatter.format(x)
        case None => value
      }
    )
}

case object Txt extends OutputFormat with Name {
  val name: String = "text"
}

case object DateStandard extends OutputFormat with Name {
  val name: String = "date"
  val formatter: DateTimeFormatter = ofPattern("d MMMM uuuu")
  override def asString(optValue: Option[String]): String =
    optValue.fold("")(value =>
      // Extract as a date, then format
      asDate(value) match {
        case Some(date) => date.format(formatter)
        case None => value
      }
    )
}

object OutputFormat {
  def apply(name: Option[String]): OutputFormat =
    name match {
      case Some(Currency.name) => Currency
      case Some(CurrencyPoundsOnly.name) => CurrencyPoundsOnly
      case Some(Txt.name) => Txt
      case Some(DateStandard.name) => DateStandard
      case _ => Txt
    }

}
