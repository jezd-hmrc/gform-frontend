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

package uk.gov.hmrc.gform.validation

import cats.implicits._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FormComponent, FormComponentId, RevealingChoice }

case class FieldOk(formComponent: FormComponent, currentValue: String) extends FormFieldValidationResult
case class FieldGlobalOk(formComponent: FormComponent, currentValue: String) extends FormFieldValidationResult
case class FieldError(formComponent: FormComponent, currentValue: String, errors: Set[String])
    extends FormFieldValidationResult
case class FieldGlobalError(formComponent: FormComponent, currentValue: String, errors: Set[String])
    extends FormFieldValidationResult
case class ComponentField(formComponent: FormComponent, data: Map[MultiFieldId, FormFieldValidationResult]) // Used by multivalue fields ie. date, address, sortcode but also choice and revealingChoice
    extends FormFieldValidationResult

trait FormFieldValidationResult {

  def forgetErrors: FormFieldValidationResult = this match {
    case t: FieldOk          => t
    case t: FieldGlobalOk    => t
    case t: FieldError       => FieldOk(t.formComponent, t.currentValue)
    case t: FieldGlobalError => FieldGlobalOk(t.formComponent, t.currentValue)
    case t: ComponentField   => ComponentField(t.formComponent, t.data.mapValues(_.forgetErrors))
  }

  lazy val fieldErrors: Set[String] = this match {
    case e: FieldError                 => e.errors
    case globalError: FieldGlobalError => globalError.errors
    case cf: ComponentField            => cf.data.values.foldLeft[Set[String]](Set())(_ ++ _.fieldErrors)
    case _                             => Set()
  }

  lazy val fieldErrorsByFieldValue: List[Set[String]] = this match {
    case e: FieldError => List(e.errors)
    case cf: ComponentField =>
      cf.data.values.toList.flatMap(_.fieldErrorsByFieldValue)
    case _ => Nil
  }

  def fieldErrorsWithSuffix(atom: Atom): Set[String] = this match {
    case ComponentField(formComponent, data) =>
      val modelComponentId: ModelComponentId = formComponent.atomicFormComponentId(atom)
      data
        .get(MultiFieldId.WhatIsThis(modelComponentId))
        .map(_.fieldErrors)
        .getOrElse(Set())
    case _ => Set()
  }

  lazy val globalErrors: Set[String] = this match {
    case e: FieldGlobalError => e.errors
    case cf: ComponentField  => cf.data.values.foldLeft[Set[String]](Set())(_ ++ _.globalErrors)
    case _                   => Set()
  }

  def formComponent: FormComponent

  def isOk: Boolean = this match {
    case FieldOk(_, _)           => true
    case FieldGlobalOk(_, _)     => true
    case ComponentField(_, data) => data.values.forall(_.isOk)
    case _                       => false
  }

  def isNotOk: Boolean = !isOk

  def getCurrentValue: Option[String] = this match {
    case FieldOk(_, "")       => None
    case FieldOk(_, cv)       => Some(cv)
    case FieldError(_, cv, _) => Some(cv)
    case _                    => None
  }

  def getOptionalCurrentValue(key: MultiFieldId): Option[String] =
    this match {
      case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue)
      case _                       => None
    }

  def getCurrentValue(key: MultiFieldId): String = getOptionalCurrentValue(key).getOrElse("")

  def getComponentFieldIndices(formComponentId: FormComponentId): List[Int] =
    this match {
      case ComponentField(_, data) =>
        data.collect {
          case (MultiFieldId.Indexed(fcId, index), _) if fcId === formComponentId => index
        }.toList
      case _ => Nil
    }

  private def withId(f: FormField, id: ModelComponentId) = f.copy(id = id)

  // Construct List[FormField] to be saved to MongoDB
  def toFormFieldY: List[FormField] = this match {
    case FieldOk(formComponent, cv)             => List(FormField(formComponent.modelComponentId, cv))
    case FieldError(formComponent, cv, _)       => List(FormField(formComponent.modelComponentId, cv))
    case FieldGlobalError(formComponent, cv, _) => List(FormField(formComponent.modelComponentId, cv))
    case FieldGlobalOk(formComponent, cv)       => List(FormField(formComponent.modelComponentId, cv))
    /* case ComponentField(formComponent, data) =>
     *   formComponent.`type` match {
     *     case _: Choice | _: RevealingChoice =>
     *       List(FormField(formComponent.id, data.keys.map(_.replace(formComponent.id.value, "")).mkString(",")))
     *     case _ => data.flatMap { case (suffix, value) => value.toFormField.map(withId(_, suffix)) }.toList
     *   } */

    case ComponentField(formComponent, data) =>
      formComponent.`type` match {
        /* data.flatMap {
         *   case (MultiFieldId.Indexed(_, index)) =>
         *   case (MultiFieldId.WhatIsThis(variadicValueId), formFieldValidationResult) =>
         * } */
        case _: Choice | _: RevealingChoice =>
          val indexes = data.keys.map(
            _.fold(whatIsThis =>
              throw new IllegalArgumentException(
                s"""Expected MultiFieldId.Indexed for id ${formComponent.modelComponentId}. Got $whatIsThis"""))(
              indexed => indexed.index))
          List(FormField(formComponent.modelComponentId, indexes.mkString(",")))
        case _ =>
          data.flatMap {
            // TODO JoVl, Eh, what is happening here?
            case (MultiFieldId.WhatIsThis(modelComponentId), formFieldValidationResult) =>
              //println("33333 modelComponentId                         : " + (modelComponentId))
              //println("33333 formFieldValidationResult.toFormField.ids: ")
              //formFieldValidationResult.toFormFieldY.foreach(println)
              val upd = formFieldValidationResult.toFormFieldY.map(withId(_, modelComponentId))
              //println("33333 formFieldValidationResult.toFormField.ids: UPDETD ")
              //upd.foreach(println)
              upd
          }.toList
      }
  }

}
