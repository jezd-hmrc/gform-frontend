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
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.foldable._
import play.api.libs.json._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.eval.ExprType
import uk.gov.hmrc.gform.gform.FormComponentUpdater
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, VerificationCodeFieldId, emailFieldId, verificationCodeFieldId }
import uk.gov.hmrc.gform.models.javascript.{ FormComponentSimple, FormComponentWithGroup, JsFormComponentModel, JsFormComponentWithCtx, JsRevealingChoiceModel }
import uk.gov.hmrc.gform.sharedmodel.{ LabelHelper, SmartString, SourceOrigin, VariadicFormData }

case class FormComponent(
  id: FormComponentId,
  `type`: ComponentType,
  label: SmartString,
  helpText: Option[SmartString],
  shortName: Option[SmartString],
  validIf: Option[ValidIf],
  mandatory: Boolean,
  editable: Boolean,
  submissible: Boolean,
  derived: Boolean,
  onlyShowOnSummary: Boolean = false,
  errorMessage: Option[SmartString],
  presentationHint: Option[List[PresentationHint]] = None,
  validators: List[FormComponentValidator] = Nil
) {

  val modelComponentId: ModelComponentId = id.modelComponentId

  val baseComponentId: BaseComponentId = id.baseComponentId

  def atomicFormComponentId(atom: Atom): ModelComponentId.Atomic = id.toAtomicFormComponentId(atom)

  def childrenFormComponents: List[FormComponent] = `type` match {
    case t: RevealingChoice => t.options.toList.flatMap(_.revealingFields)
    case t: Group           => t.fields
    case _                  => Nil
  }

  def lookupFor: Map[ModelComponentId, FormComponent] = multiValueId.lookupFor(this)

  val multiValueId: MultiValueId = modelComponentId.fold(
    pure =>
      this match {
        case IsMultiField(multifield) => MultiValueId.multiValue(pure, multifield.fields(pure.indexedComponentId))
        case _                        => MultiValueId.pure(pure)
    }
  )(
    atomic => throw new IllegalArgumentException(s"$atomic cannot be broken into multiValues")
  )

  def firstAtomModelComponentId: ModelComponentId.Atomic = multiValueId.firstAtomModelComponentId

  def getType: ExprType = this match {
    case IsText(Text(Sterling(_, _), _, _, _))             => ExprType.Sterling
    case IsText(Text(Number(_, _, _, _), _, _, _))         => ExprType.Number
    case IsText(Text(PositiveNumber(_, 0, _, _), _, _, _)) => ExprType.WholeNumber
    case IsText(Text(PositiveNumber(_, _, _, _), _, _, _)) => ExprType.Number
    case IsChoice(_)                                       => ExprType.ChoiceSelection
    case IsRevealingChoice(_)                              => ExprType.ChoiceSelection
    case _                                                 => ExprType.String
  }

  def hideOnSummary: Boolean =
    presentationHint.fold(false)(x => x.contains(InvisibleInSummary)) || IsInformationMessage.unapply(this).isDefined

  /* private def updateField(i: Int, fc: FormComponent): FormComponent =
   *   fc.copy(
   *     label = LabelHelper.buildRepeatingLabel(fc.label, i),
   *     shortName = LabelHelper.buildRepeatingLabel(fc.shortName, i)) */

  /* private def loop(fc: FormComponent): List[FormComponent] =
   *   fc.`type` match {
   *     case Group(fields, max, _, _, _) =>
   *       val expandedFields =
   *         for {
   *           field <- fields
   *           res <- updateField(1, field) :: (1 until max.getOrElse(1))
   *                   .map(i => updateField(i + 1, field.copy(id = FormComponentId(i + "_" + field.id.value))))
   *                   .toList
   *         } yield res
   *       expandedFields.flatMap(loop) // for case when there is group inside group (Note: it does not work, we would need to handle prefix)
   *     case RevealingChoice(options, _) => fc :: options.toList.foldMap(_.revealingFields.map(loop)).flatten
   *     case _                           => fc :: Nil
   *   } */

  //lazy val expandedFormComponents: List[FormComponent] = loop(this)

  /* private def addFieldIndex(field: FormComponent, index: Int, group: Group) = {
   *   val fieldToUpdate = if (index === 0) field else field.copy(id = FormComponentId(index + "_" + field.id.value))
   *   val i = index + 1
   *   FormComponentUpdater(
   *     fieldToUpdate.copy(
   *       label = LabelHelper.buildRepeatingLabel(field.label, i),
   *       shortName = LabelHelper.buildRepeatingLabel(field.shortName, i)
   *     ),
   *     index,
   *     group
   *   ).updatedWithId
   * } */

  /* def expandGroup[S <: SourceOrigin](data: VariadicFormData[S]): Group => Int => Group =
   *   group =>
   *     index => {
   *       val ids: List[FormComponentId] = groupIndex(index + 1, group)
   *       val toExpand: Boolean = ids.forall(data.contains) // TODO JoVl gorup expanding implementation
   *       if (index === 0 || toExpand) {
   *         group.fields.map(addFieldIndex(_, index, group))
   *       } else Nil
   *   } */

  /* private def mkJsFormComponentModels(fc: FormComponent): List[JsFormComponentModel] =
   *   fc.`type` match {
   *     case RevealingChoice(options, _) =>
   *       options.toList.flatMap { option =>
   *         option.revealingFields.map(rf => JsRevealingChoiceModel(fc.id, rf))
   *       }
   *     case group @ Group(fields, max, _, _, _) =>
   *       (0 until max.getOrElse(1)).toList.flatMap(index =>
   *         fields.map(field => JsFormComponentWithCtx(FormComponentWithGroup(addFieldIndex(field, index, group), fc))))
   *     case _ => JsFormComponentWithCtx(FormComponentSimple(fc)) :: Nil
   *   } */

  /* def expandFormComponent[S <: SourceOrigin](group: Group)(data: VariadicFormData[S]): List[FormComponent] =
   *   expandByData(this, group, data) */

  //val jsFormComponentModels: List[JsFormComponentModel] = mkJsFormComponentModels(this)
  val jsFormComponentModels: List[JsFormComponentModel] = List.empty[JsFormComponentModel]

}

object FormComponent {
  implicit val format: OFormat[FormComponent] = Json.format[FormComponent]
}

object IsText {
  def unapply(fc: FormComponent): Option[Text] = fc.`type`.cast[Text]
}

object IsCapitalised {
  def unapply(fc: FormComponent): Boolean =
    fc.`type` match {
      case t @ Text(_, _, _, IsUpperCase) => true
      case _                              => false
    }
}

object IsTextArea {
  def unapply(fc: FormComponent): Option[TextArea] = fc.`type`.cast[TextArea]
}

object IsGroup {
  def unapply(fc: FormComponent): Option[Group] = fc.`type`.cast[Group]
}

object IsMultiField {
  def unapply(fc: FormComponent): Option[MultiField] = fc.`type`.cast[MultiField]
}

object IsDate {
  def unapply(fc: FormComponent): Option[Date] = fc.`type`.cast[Date]
}

object IsChoice {
  def unapply(fc: FormComponent): Option[Choice] = fc.`type`.cast[Choice]
}

object IsRevealingChoice {
  def unapply(fc: FormComponent): Option[RevealingChoice] = fc.`type`.cast[RevealingChoice]
}

object IsAddress {
  def unapply(fc: FormComponent): Option[Address] = fc.`type`.cast[Address]
}

object IsUkSortCode {
  def unapply(fc: FormComponent): Option[UkSortCode] = fc.`type`.cast[UkSortCode]
}

object IsInformationMessage {
  def unapply(fc: FormComponent): Option[InformationMessage] = fc.`type`.cast[InformationMessage]
}

object IsHmrcTaxPeriod {
  def unapply(fc: FormComponent): Option[HmrcTaxPeriod] = fc.`type`.cast[HmrcTaxPeriod]
}

object IsFileUpload {
  def unapply(fc: FormComponent): Boolean = fc.`type`.cast[FileUpload].isDefined
}

object IsEmailVerifier {
  def unapply(formComponent: FormComponent): Option[(EmailFieldId, EmailVerifiedBy)] =
    formComponent.`type` match {
      case Text(evb @ EmailVerifiedBy(_, _), _, _, _) =>
        Some((emailFieldId(formComponent.id), evb))
      case _ => None
    }
}

object AllValidIfs {
  def unapply(fc: FormComponent): Option[List[ValidIf]] = (fc.validIf, fc.validators.map(_.validIf)) match {
    case (None, Nil)     => None
    case (None, xs2)     => Some(xs2)
    case (Some(xs), xs2) => Some(xs :: xs2)
  }
}
