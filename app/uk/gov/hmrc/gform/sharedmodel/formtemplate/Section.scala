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

sealed trait BaseSection {
  def title: SmartString
  def shortName: Option[SmartString]
  def fields: List[FormComponent]
}

case class ExpandedSection(expandedFormComponents: List[ExpandedFormComponent], includeIf: Option[IncludeIf]) {
  def toExpandedFormTemplate: ExpandedFormTemplate = ExpandedFormTemplate(this :: Nil)
  def allFCs: List[FormComponent] = toExpandedFormTemplate.allFormComponents
}

sealed trait TemporarySectionOpsForGforms364 extends BaseSection {
  def description: Option[SmartString]
  def includeIf: Option[IncludeIf]

  def expandSection(data: VariadicFormData): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponent(data)), includeIf) // TODO expand sections

  def expandSectionRc(data: VariadicFormData): ExpandedSection =
    ExpandedSection(fields.map(_.expandFormComponentRc(data)), includeIf) // TODO expand sections

}

sealed trait Section extends BaseSection with TemporarySectionOpsForGforms364 with Product with Serializable {
  def title: SmartString
  def expandSectionFull(): List[ExpandedSection]
  def expandedFormComponents(): List[FormComponent]

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

  def jsFormComponentModels: List[JsFormComponentModel] = fields.flatMap(_.jsFormComponentModels)
}

object Section {
  case class NonRepeatingPage(page: Page) extends Section {
    override def title: SmartString = page.title
    override def description: Option[SmartString] = page.description
    override def shortName: Option[SmartString] = page.shortName
    override def fields: List[FormComponent] = page.fields
    override def includeIf: Option[IncludeIf] = page.includeIf

    override def expandSectionFull(): List[ExpandedSection] =
      ExpandedSection(page.fields.map(_.expandFormComponentFull), page.includeIf) :: Nil
    override def expandedFormComponents(): List[FormComponent] = page.expandedFormComponents
  }

  case class RepeatingPage(page: Page, repeats: TextExpression) extends Section {
    override def title: SmartString = page.title
    override def description: Option[SmartString] = page.description
    override def shortName: Option[SmartString] = page.shortName
    override def fields: List[FormComponent] = page.fields
    override def includeIf: Option[IncludeIf] = page.includeIf

    override def expandSectionFull(): List[ExpandedSection] =
      ExpandedSection(page.fields.map(_.expandFormComponentFull), page.includeIf) :: Nil // TODO expand repeats
    override def expandedFormComponents(): List[FormComponent] = page.expandedFormComponents
  }

  case class AddToList(
    id: AddToListId,
    title: SmartString,
    description: Option[SmartString],
    shortName: Option[SmartString],
    includeIf: Option[IncludeIf],
    repeatsMax: Option[TextExpression],
    pages: NonEmptyList[Page]
  ) extends Section {

    // ToDo Lance - Some of these need to be implemented and some need to be removed
    override def expandSectionFull(): List[ExpandedSection] =
      pages.toList.map(page => ExpandedSection(page.fields.map(_.expandFormComponentFull), page.includeIf))
    override def fields: List[FormComponent] = pages.toList.flatMap(_.expandedFormComponents)

    override lazy val expandedFormComponents: List[FormComponent] = pages.toList.flatMap(_.expandedFormComponents)
  }

  implicit val format: OFormat[Section] = derived.oformat[Section]
}

case class DeclarationSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) extends BaseSection

object DeclarationSection {
  implicit val format: OFormat[DeclarationSection] = Json.format[DeclarationSection]
}

case class AcknowledgementSection(
  title: SmartString,
  description: Option[SmartString],
  shortName: Option[SmartString],
  fields: List[FormComponent]
) extends BaseSection

object AcknowledgementSection {
  implicit val format: OFormat[AcknowledgementSection] = Json.format[AcknowledgementSection]
}

case class EnrolmentSection(
  title: SmartString,
  shortName: Option[SmartString],
  fields: List[FormComponent],
  identifiers: NonEmptyList[IdentifierRecipe],
  verifiers: List[VerifierRecipe]
) extends BaseSection

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
