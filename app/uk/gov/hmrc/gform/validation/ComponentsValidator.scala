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

package uk.gov.hmrc.gform.validation

import cats.data.Validated
import cats.implicits._
import play.api.i18n.Messages

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.{ Error, File, FileUploadService, Infected }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormDataRecalculated, Seed, ThirdPartyData, Validated => _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

class ComponentsValidator(
  data: FormDataRecalculated,
  fileUploadService: FileUploadService,
  seed: Seed,
  envelopeId: EnvelopeId,
  retrievals: MaterialisedRetrievals,
  booleanExpr: BooleanExprEval[Future],
  thirdPartyData: ThirdPartyData,
  formTemplate: FormTemplate,
  lookupRegistry: LookupRegistry)(
  implicit ec: ExecutionContext,
  messages: Messages
) {

  val cvh = new ComponentsValidatorHelper()
  val dateValidation = new DateValidation()

  def validate(fieldValue: FormComponent, fieldValues: List[FormComponent])(
    implicit hc: HeaderCarrier,
    messages: Messages): Future[ValidatedType[Unit]] = {

    def validIf(validationResult: ValidatedType[Unit]): Future[ValidatedType[Unit]] =
      (validationResult.isValid, fieldValue.validIf) match {
        case (true, Some(vi)) =>
          booleanExpr
            .isTrue(vi.expr, data.data, retrievals, data.invisible, thirdPartyData, seed, formTemplate)
            .map {
              case false => validationFailure(fieldValue, messages("generic.error.required"))
              case true  => validationResult
            }
        case _ => validationResult.pure[Future]
      }

    fieldValue.`type` match {
      case sortCode @ UkSortCode(_) =>
        validIf(
          SortCodeValidation
            .validateSortCode(fieldValue, sortCode, fieldValue.mandatory)(data))
      case date @ Date(_, _, _) =>
        validIf(dateValidation.validateDate(fieldValue, date, getCompanionFieldComponent(date, fieldValues), data))
      case text @ Text(constraint, _, _, _) =>
        validIf(
          ComponentValidator
            .validateText(fieldValue, constraint, retrievals)(data, lookupRegistry))
      case TextArea(constraint, _, _) =>
        validIf(
          ComponentValidator
            .validateText(fieldValue, constraint, retrievals)(data, lookupRegistry))
      case address @ Address(_) => validIf(new AddressValidation().validateAddress(fieldValue, address)(data))
      case c @ Choice(_, _, _, _, _) =>
        validIf(ComponentValidator.validateChoice(fieldValue)(data))
      case Group(_, _, _, _, _, _) => cvh.validF //a group is read-only
      // case FileUpload()             => validateFileUpload(data, fieldValue) //TODO: need to keep this in for MDTP
      case InformationMessage(_, _) => cvh.validF
      case HmrcTaxPeriod(_, _, _) =>
        validIf(ComponentValidator.validateChoice(fieldValue)(data))
    }
  }

  //TODO: this will be called many times per one form. Maybe there is a way to optimise it?
  private def validateFileUpload(data: FormDataRecalculated, fieldValue: FormComponent)(
    implicit hc: HeaderCarrier,
    messages: Messages): Future[ValidatedType[Unit]] =
    fileUploadService
      .getEnvelope(envelopeId)
      .map { envelope =>
        val fileId = FileId(fieldValue.id.value)
        val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

        file match {
          case Some(File(fileId, Error(Some(reason)), _)) =>
            validationFailure(fieldValue, reason)
          case Some(File(fileId, Error(None), _)) =>
            validationFailure(fieldValue, messages("generic.error.unknownUpload"))
          case Some(File(fileId, Infected, _)) =>
            validationFailure(fieldValue, messages("generic.error.virus"))
          case Some(File(fileId, _, _)) => ValidationServiceHelper.validationSuccess
          case None if fieldValue.mandatory =>
            validationFailure(fieldValue, messages("generic.error.upload"))
          case None => validationSuccess
        }
      }
}

class ComponentsValidatorHelper(implicit messages: Messages) {

  def validF(implicit ec: ExecutionContext) =
    ValidationServiceHelper.validationSuccess.pure[Future]

  def validateRF(fieldValue: FormComponent, value: String) =
    validateRequired(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateFF(fieldValue: FormComponent, value: String) =
    validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateRequired(fieldValue: FormComponent, fieldId: FormComponentId, errorPrefix: Option[String] = None)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map(
          fieldId -> ComponentsValidatorHelper
            .errors(fieldValue, messages("field.error.required", errorPrefix.getOrElse("")))).invalid
      case value :: Nil  => validationSuccess
      case value :: rest => validationSuccess // we don't support multiple values yet
    }

  def validateForbidden(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil => validationSuccess
      case value :: Nil =>
        Map(fieldId -> ComponentsValidatorHelper.errors(fieldValue, messages("generic.error.forbidden"))).invalid
      case value :: rest =>
        Map(fieldId -> ComponentsValidatorHelper.errors(fieldValue, messages("generic.error.forbidden"))).invalid // we don't support multiple values yet
    }
}

object ComponentsValidatorHelper {

  def messagePrefix(fieldValue: FormComponent, workedOnId: FormComponentId, otherFormComponent: Option[FormComponent]) =
    otherFormComponent match {
      case Some(x) if x.id === workedOnId => x.shortName.getOrElse(x.label)
      case Some(x)                        => fieldValue.shortName.getOrElse(fieldValue.label)
      case None                           => fieldValue.shortName.getOrElse(fieldValue.label)
    }

  def errors(fieldValue: FormComponent, defaultErr: String): Set[String] =
    Set(fieldValue.errorMessage.getOrElse(messagePrefix(fieldValue, fieldValue.id, None) + " " + defaultErr))

  def getError(
    fieldValue: FormComponent,
    defaultMessage: String): Validated[Map[FormComponentId, Set[String]], Nothing] =
    Map(fieldValue.id -> errors(fieldValue, defaultMessage)).invalid
}
