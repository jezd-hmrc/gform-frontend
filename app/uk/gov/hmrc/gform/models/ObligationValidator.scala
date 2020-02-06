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
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.{ Obligations, RetrievedObligations, TaxResponse }

trait ObligationValidator extends TaxSelectionNavigator {

  def validateWithDes(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    cachedObligation: Obligations,
    desObligation: Obligations,
    clearTaxResponses: FormModelOptics[DataOrigin.Browser] => FormModelOptics[DataOrigin.Browser])
    : FormModelOptics[DataOrigin.Browser] =
    (cachedObligation, desObligation) match {
      case (RetrievedObligations(obligation), RetrievedObligations(responseObligation))
          if mayClear(formModelOptics, obligation, responseObligation) =>
        clearTaxResponses(formModelOptics)
      case _ => formModelOptics
    }

  private def mayClear(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    cachedObligation: NonEmptyList[TaxResponse],
    responseObligation: NonEmptyList[TaxResponse]): Boolean =
    cachedObligation.toList
      .zip(responseObligation.toList)
      .map {
        case (cached, taxResponse) => (cached.obligation, taxResponse)
      }
      .map { case (obligation, taxResponse) => taxSelectionNavigator(formModelOptics, obligation, taxResponse) }
      .count(_ == GoBackToTaxPeriodSelection) > 0
}
