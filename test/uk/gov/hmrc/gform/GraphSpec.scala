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

package uk.gov.hmrc.gform

import cats.Monad
import cats.syntax.applicative._
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest

import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.{ BooleanExprEval, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.http.HeaderCarrier

trait GraphSpec {

  private def eligibilityStatusTrue[F[_]: Monad]: SeissEligibilityChecker[F] =
    new SeissEligibilityChecker[F]((_, _) => true.pure[F])

  private def eligibilityStatusFalse[F[_]: Monad]: SeissEligibilityChecker[F] =
    new SeissEligibilityChecker[F]((_, _) => false.pure[F])

  def booleanExprEval[F[_]: Monad]: BooleanExprEval[F] = new BooleanExprEval[F](eligibilityStatusTrue[F])

  def booleanExprEval2[F[_]: Monad]: BooleanExprEval[F] = new BooleanExprEval[F](eligibilityStatusFalse[F])

  /* protected def mkFormDataRecalculated(data: VariadicFormData[SourceOrigin.OutOfDate]): FormDataRecalculated =
 *   FormDataRecalculated.empty.copy(recData = RecData.fromData(data)) */

}
