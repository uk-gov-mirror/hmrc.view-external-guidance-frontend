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

package core.models.ocelot

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Label(name: String, english: Option[String] = None, welsh: Option[String] = None)
case class ListLabel(name: String, english: Option[List[String]], welsh: Option[List[String]] = None)

object Label {
  implicit val reads: Reads[Label] = (
    (__ \ "name").read[String] and
      (__ \ "english").readNullable[String] and
      (__ \ "welsh").readNullable[String]
  )(Label.apply _)

  implicit val writes: Writes[Label] = (
    (__ \ "name").write[String] and
      (__ \ "english").writeNullable[String] and
      (__ \ "welsh").writeNullable[String]
  )(unlift(Label.unapply))
}

object ListLabel {

  def buildListLabel(name: String, english: Option[String], welsh: Option[String]) : ListLabel = {

    def stringToList(s: Option[String]): Option[List[String]] = s match {
      case Some(value) if value == "" => Some(Nil)
      case Some(value) => Some(value.split(",").map(_.trim).toList)
      case _ => None
    }

    ListLabel(name, stringToList(english), stringToList(welsh))

  }

  implicit val reads: Reads[ListLabel] = (
    (__ \ "name").read[String] and
      (__ \ "english").readNullable[String] and
      (__ \ "welsh").readNullable[String]
  )(ListLabel.buildListLabel _)

  def deconstructListLabel(listLabel: ListLabel): Option[(String, Option[String], Option[String])] = {

    def listToString(list: Option[List[String]]): Option[String] = list match {
      case Some(value) => value match {
        case Nil => Some("")
        case list => Some(list.mkString(","))
      }
      case _ => None
    }

    Some((listLabel.name, listToString(listLabel.english), listToString(listLabel.welsh)))
  }

  implicit val writes: Writes[ListLabel] = (
    (__ \ "name").write[String] and
      (__ \ "english").writeNullable[String] and
      (__ \ "welsh").writeNullable[String]
  )(unlift(ListLabel.deconstructListLabel))

}