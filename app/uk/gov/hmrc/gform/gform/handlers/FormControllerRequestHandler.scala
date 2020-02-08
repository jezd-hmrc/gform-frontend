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

import cats.syntax.validated._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.{ FormModel, ProcessData }
import uk.gov.hmrc.gform.models.gform.{ FormComponentValidation, FormValidationOutcome }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormTemplate, FullyExpanded, SeNo, SeYes, Section, SectionNumber, SuppressErrors }
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormControllerRequestHandler(formValidator: FormValidator)(implicit ec: ExecutionContext) {

  def handleSuppressErrors(
    sectionNumber: SectionNumber,
    suppressErrors: SuppressErrors,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    data: FormDataRecalculated,
    formModel: FormModel[FullyExpanded],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(implicit hc: HeaderCarrier)
    : Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] =
    suppressErrors match {
      case SeYes =>
        Future.successful((List.empty, ValidationResult.empty.valid, envelope))
      case SeNo =>
        handleValidate(
          data,
          formModel,
          sectionNumber,
          cache.form.envelopeId,
          envelope,
          retrievals,
          cache.form.thirdPartyData,
          cache.formTemplate,
          validateFormComponents,
          evaluateValidation
        )
    }

  def handleForm(
    sectionNumber: SectionNumber,
    suppressErrors: SuppressErrors,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    recalculateDataAndSections: RecalculateDataAndSections[Future],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(implicit hc: HeaderCarrier): Future[FormHandlerResult] = {
    val retrievals = cache.retrievals

    for {
      (data, formModel) <- recalculateDataAndSections(cache.variadicFormData, cache)
      (errors, validate, envelope) <- handleSuppressErrors(
                                       sectionNumber,
                                       suppressErrors,
                                       cache,
                                       envelope,
                                       retrievals,
                                       data,
                                       formModel,
                                       validateFormComponents,
                                       evaluateValidation
                                     )
    } yield FormHandlerResult(data, errors, envelope, validate, formModel)
  }

  def handleFormValidation(
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
    formValidator.validateForm(
      data,
      formModel,
      sn,
      cache,
      envelope,
      extractedValidateFormHelper,
      validateFormComponents,
      evaluateValidation)

  def handleFastForwardValidate(
    processData: ProcessData,
    cache: AuthCacheWithForm,
    envelope: Envelope,
    extractedValidateFormHelper: (
      List[FormComponentValidation],
      ValidatedType[ValidationResult]) => FormValidationOutcome,
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation)(
    implicit hc: HeaderCarrier
  ): Future[Option[SectionNumber]] =
    formValidator.fastForwardValidate(
      processData,
      cache,
      envelope,
      extractedValidateFormHelper,
      validateFormComponents,
      evaluateValidation)

  def handleValidate(
    formDataRecalculated: FormDataRecalculated,
    formModel: FormModel[FullyExpanded],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation)(
    implicit hc: HeaderCarrier
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] =
    formValidator.validate(
      formDataRecalculated,
      formModel,
      sectionNumber,
      envelopeId,
      envelope,
      retrievals,
      thirdPartyData,
      formTemplate,
      validateFormComponents,
      evaluateValidation)
}

case class FormHandlerResult(
  data: FormDataRecalculated,
  result: List[(FormComponent, FormFieldValidationResult)],
  envelope: Envelope,
  validatedType: ValidatedType[ValidationResult],
  formModel: FormModel[FullyExpanded])
