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

package uk.gov.hmrc.gform.gform.handlers

import cats.syntax.eq._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, Origin }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.ExpandUtils.{ nonSubmittedFCsOfNonGroup, submittedFCs }
import uk.gov.hmrc.gform.models.{ FastForward, FormModel, ProcessData }
import uk.gov.hmrc.gform.models.gform.{ FormComponentValidation, FormValidationOutcome }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ EmailCodeFieldMatcher, FormFieldValidationResult, GetEmailCodeFieldMatcher }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class FormValidator(implicit ec: ExecutionContext) {

  def validateForm(
    data: FormDataRecalculated,
    formModel: FormModel[FullyExpanded],
    sn: SectionNumber,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    extractedValidateFormHelper: (
      List[FormComponentValidation],
      ValidatedType[ValidationResult]) => FormValidationOutcome,
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(
    implicit hc: HeaderCarrier
  ): Future[FormValidationOutcome] =
    validate(
      data,
      formModel,
      sn,
      cache.form.envelopeId,
      envelope,
      cache.retrievals,
      cache.form.thirdPartyData,
      cache.formTemplate,
      validateFormComponents,
      evaluateValidation
    ).map {
      case (validationResult, validatedType, _) =>
        val fcvs: List[FormComponentValidation] = validationResult.map {
          case (formComponent, formFieldValidationResult) =>
            FormComponentValidation(formComponent, formFieldValidationResult)
        }
        extractedValidateFormHelper(fcvs, validatedType)
    }

  def validate(
    formDataRecalculated: FormDataRecalculated,
    formModel: FormModel[FullyExpanded],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(
    implicit hc: HeaderCarrier
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] = {
    val pageModel = formModel(sectionNumber)
    val nonSubmittedYet = nonSubmittedFCsOfNonGroup(formDataRecalculated, pageModel)
    //val allFC = submittedFCs(formDataRecalculated, formModel.flatMap(_.expandSection(formDataRecalculated.data).allFCs)) ++ nonSubmittedYet
    val allFC: List[FormComponent] = submittedFCs(formDataRecalculated, formModel.allFormComponents) ++ nonSubmittedYet
    //val sectionFields = submittedFCs(formDataRecalculated, section.expandSectionRc(formDataRecalculated.data).allFCs) ++ nonSubmittedYet
    val sectionFields
      : List[FormComponent] = submittedFCs(formDataRecalculated, pageModel.allFormComponents) ++ nonSubmittedYet

    for {
      v <- validateFormComponents(
            sectionFields,
            pageModel,
            envelopeId,
            envelope,
            retrievals,
            thirdPartyData,
            formTemplate,
            formDataRecalculated,
            GetEmailCodeFieldMatcher(formModel))
    } yield (evaluateValidation(v, allFC, formDataRecalculated, envelope), v, envelope)
  }

  def fastForwardValidate(
    processData: ProcessData,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    extractedValidateFormHelper: (
      List[FormComponentValidation],
      ValidatedType[ValidationResult]) => FormValidationOutcome,
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation,
    fastForward: FastForward
  )(
    implicit hc: HeaderCarrier
  ): Future[Option[SectionNumber]] = {

    val formModel: FormModel[FullyExpanded] = processData.formModel
    val data = processData.data

    val availableSectionNumbers: List[SectionNumber] = Origin(formModel, data).availableSectionNumbers
    availableSectionNumbers.foldLeft(Future.successful(None: Option[SectionNumber])) {
      case (accF, currentSn) =>
        accF.flatMap {
          case Some(sn) => Future.successful(Some(sn))
          case None =>
            validateForm(
              data,
              formModel,
              currentSn,
              cache,
              envelope,
              extractedValidateFormHelper,
              validateFormComponents,
              evaluateValidation)
              .map {
                case FormValidationOutcome(isValid, _, _) =>
                  val page = formModel(currentSn)
                  val hasBeenVisited = processData.visitsIndex.contains(currentSn.value)

                  val stop = page.isTerminationPage || !hasBeenVisited
                  if (isValid && !stop && fastForward.goOn(currentSn)) None else Some(currentSn)
              }
        }
    }
  }

}
