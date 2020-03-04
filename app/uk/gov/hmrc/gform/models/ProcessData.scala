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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ Monad, MonadError }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation }
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FullyExpanded, Section }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.models.gform.ObligationsAction

import scala.util.Try

case class ProcessData(data: FormDataRecalculated, visitsIndex: VisitIndex, obligations: Obligations) {
  val formModel: FormModel[FullyExpanded, SourceOrigin.Current] = data.formModel
}

class ProcessDataService[F[_]: Monad, E](recalculation: Recalculation[F, E]) {

  def hmrcTaxPeriodWithId(recData: RecData): Option[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]] =
    recData.recalculatedTaxPeriod.map {
      case (periodKey, idNumberValue) =>
        HmrcTaxPeriodWithEvaluatedId(periodKey, idNumberValue)
    } match {
      case x :: xs => Some(NonEmptyList(x, xs))
      case Nil     => None
    }

  def getProcessData(
    dataRaw: VariadicFormData[SourceOrigin.OutOfDate],
    cache: AuthCacheWithForm,
    getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => F[NonEmptyList[TaxResponse]],
    obligationsAction: ObligationsAction
  )(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E],
    l: LangADT
  ): F[ProcessData] =
    for {
      browserRecalculated <- recalculateDataAndSections(dataRaw, cache)
      mongoRecalculated   <- recalculateDataAndSections(cache.variadicFormData, cache)
      data = browserRecalculated
      mongoData = mongoRecalculated
      obligations <- new TaxPeriodStateChecker[F]().callDesIfNeeded(
                      getAllTaxPeriods,
                      hmrcTaxPeriodWithId(data.recData),
                      cache.form.thirdPartyData.obligations,
                      data.recData.recalculatedTaxPeriod,
                      mongoData.recData.recalculatedTaxPeriod,
                      obligationsAction
                    )

    } yield {

      val dataUpd = new ObligationValidator {}.validateWithDes(
        data,
        cache.form.thirdPartyData.obligations,
        obligations,
        FormDataRecalculated.clearTaxResponses)

      val newVisitIndex =
        VisitIndex.updateSectionVisits(data.formModel, mongoData.formModel, cache.form.visitsIndex)

      ProcessData(dataUpd, VisitIndex(newVisitIndex), obligations)
    }

  def recalculateDataAndSections(data: VariadicFormData[SourceOrigin.OutOfDate], cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier,
    me: MonadError[F, E]
  ): F[FormDataRecalculated] =
    for {
      formDataRecalculated <- recalculation
                               .recalculateFormData(
                                 data,
                                 cache.formTemplate,
                                 cache.retrievals,
                                 cache.form.thirdPartyData,
                                 cache.form.envelopeId)
    } yield formDataRecalculated

}
