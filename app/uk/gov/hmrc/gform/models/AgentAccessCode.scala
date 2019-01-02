/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.data.State
import play.api.data.Forms.{ mapping, nonEmptyText }
import play.api.data.Mapping
import play.api.data.validation.Constraints
import uk.gov.hmrc.gform.gform.AccessCodeForm
import uk.gov.hmrc.gform.models.MappingsApi.{ MappingOps, MappingWithKeyOps }
import uk.gov.hmrc.gform.typeclasses.Rnd

case class AgentAccessCode(value: String) extends AnyVal

object AgentAccessCode {

  val key = "accessCode"
  val optionKey = "accessOption"
  val optionAccess = "access"
  val optionNew = "new"

  def random(implicit rnd: Rnd[Int]): AgentAccessCode = {

    def getChars(i: Int) = State[Rnd[Int], String](r => (r, alphanumeric(r).take(i).toList.mkString))

    val refGen = for {
      a <- getChars(3)
      b <- getChars(4)
      c <- getChars(3)
    } yield AgentAccessCode(a + "-" + b + "-" + c)

    refGen.runA(rnd).value
  }

  private def alphanumeric(rnd: Rnd[Int]): Stream[Char] = {
    val chars = ('A' to 'Z') ++ ('0' to '9').toList
    def nextAlphaNum: Char = chars.charAt(rnd.random(chars.length))
    Stream continually nextAlphaNum
  }

  val optionMapping: (String, Mapping[String]) = AgentAccessCode.optionKey -> nonEmptyText
  def form: play.api.data.Form[AccessCodeForm] =
    play.api.data.Form(
      mapping(
        AgentAccessCode.key → (accessCodeFormat onlyWhen (optionMapping is AgentAccessCode.optionAccess)),
        optionMapping
      )(AccessCodeForm.apply)(AccessCodeForm.unapply))

  def accessCodeFormat: Mapping[String] =
    nonEmptyText.verifying(
      Constraints.pattern(
        "^[A-Z0-9]{3}-[A-Z0-9]{4}-[A-Z0-9]{3}$".r
      )
    )
}
