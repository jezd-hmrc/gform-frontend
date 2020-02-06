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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.syntax.option._
import julienrf.json.derived
import play.api.libs.json._

sealed trait DataSource {
  def convertToString(): String = this match {
    case DataSource.SeissEligible     => DataSource.seiss
    case DataSource.Mongo(collection) => DataSource.mongoPrefix + collection
  }
}

object DataSource {
  case object SeissEligible extends DataSource
  case class Mongo(collectionName: String) extends DataSource

  implicit val format: OFormat[DataSource] = derived.oformat

  def fromString(str: String): Option[DataSource] = str match {
    case `seiss`                                          => DataSource.SeissEligible.some
    case maybeMongo if maybeMongo.startsWith(mongoPrefix) => DataSource.Mongo(maybeMongo.replace(mongoPrefix, "")).some
    case _                                                => none
  }

  val seiss = "seiss"
  val mongoPrefix = "mongo."

}
