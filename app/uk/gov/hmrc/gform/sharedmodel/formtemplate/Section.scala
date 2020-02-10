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

import cats.data.NonEmptyList
import cats.instances.list._
import cats.syntax.foldable._
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.models.javascript.JsFormComponentModel
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, VariadicFormData }

import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.nelFormat

sealed trait Section extends Product with Serializable {
  def getTitle: SmartString = this match {
    case Section.NonRepeatingPage(page)             => page.title
    case Section.RepeatingPage(page, _)             => page.title
    case Section.AddToList(_, title, _, _, _, _, _) => title
  }

  def validators: Option[Validator] = this match {
    case Section.NonRepeatingPage(page)         => page.validators
    case Section.RepeatingPage(page, _)         => page.validators
    case Section.AddToList(_, _, _, _, _, _, _) => None
  }

  def progressIndicator: Option[SmartString] = this match {
    case Section.NonRepeatingPage(page)         => page.progressIndicator
    case Section.RepeatingPage(page, _)         => page.progressIndicator
    case Section.AddToList(_, _, _, _, _, _, _) => None
  }

  def continueLabel: Option[SmartString] = this match {
    case Section.NonRepeatingPage(page)         => page.continueLabel
    case Section.RepeatingPage(page, _)         => page.continueLabel
    case Section.AddToList(_, _, _, _, _, _, _) => None
  }

  def isRepeating: Boolean = this match {
    case Section.NonRepeatingPage(_)            => false
    case Section.RepeatingPage(_, _)            => true
    case Section.AddToList(_, _, _, _, _, _, _) => false
  }

  def isTerminationPage: Boolean = this match {
    case Section.NonRepeatingPage(page)         => page.continueIf.contains(Stop)
    case Section.RepeatingPage(page, _)         => page.continueIf.contains(Stop)
    case Section.AddToList(_, _, _, _, _, _, _) => true
  }
}

object Section {
  case class NonRepeatingPage(page: Page[Basic]) extends Section

  case class RepeatingPage(page: Page[Basic], repeats: TextExpression) extends Section

  case class AddToList(
    id: AddToListId,
    title: SmartString,
    description: Option[SmartString],
    shortName: Option[SmartString],
    includeIf: Option[IncludeIf],
    repeatsMax: Option[TextExpression],
    pages: NonEmptyList[Page[Basic]]
  ) extends Section

  implicit val format: OFormat[Section] = derived.oformat[Section]
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = description,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None
    )
}

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) {

  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = description,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None
    )
}

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: SmartString,
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) {
  def toSection = Section.NonRepeatingPage(toPage)

  def toPage: Page[Basic] =
    Page(
      title = title,
      description = None,
      shortName = shortName,
      progressIndicator = None,
      includeIf = None,
      validators = None,
      fields = fields,
      continueLabel = None,
      continueIf = None
    )
}

object EnrolmentSection {
  import JsonUtils._
  implicit val format: OFormat[EnrolmentSection] = Json.format[EnrolmentSection]
}

case class IdentifierRecipe(key: String, value: FormCtx)
object IdentifierRecipe {
  implicit val format: OFormat[IdentifierRecipe] = Json.format[IdentifierRecipe]
}

case class VerifierRecipe(key: String, value: FormCtx)
object VerifierRecipe {
  implicit val format: OFormat[VerifierRecipe] = Json.format[VerifierRecipe]
}
