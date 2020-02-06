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

package uk.gov.hmrc.gform.models.optics

import uk.gov.hmrc.gform.eval.{ EvaluationResults, ExpressionResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ FormModel, PageModel, Visibility }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsMultiField, SectionNumber }

class FormModelVisibilityOptics[D <: DataOrigin](
  val formModel: FormModel[Visibility],
  val recData: RecData[SourceOrigin.Current],
  val evaluationResults: EvaluationResults,
  val graphTopologicalOrder: Traversable[(Int, List[GraphNode])],
  val booleanExprCache: BooleanExprCache
) {

  def allFormComponents: List[FormComponent] = formModel.allFormComponents

  def allFormComponentIds: List[FormComponentId] =
    allFormComponents.map(_.id)

  /* def get(atomicFcId: AtomicFormComponentId): Option[VariadicValue] =
   *   recData.variadicFormData.get(atomicFcId.toModelComponentId) */

  def collect[B](pf: PartialFunction[(ModelComponentId, VariadicValue), B]): Iterable[B] =
    recData.variadicFormData.collect(pf)

  def fcLookup: Map[FormComponentId, FormComponent] = formModel.fcLookup

  def evalO(expr: Expr): Option[ExpressionResult] = {
    val typedExpr = formModel.toTypedExpr(expr)
    evaluationResults.get(typedExpr)
  }

  def eval(expr: Expr): String = evalO(expr).fold("")(_.stringRepresentation)

  object data {
    def all: List[VariadicValue] = allFormComponents.flatMap(_.multiValueId.toModelComponentIds).flatMap { fcId =>
      get(fcId)
    }
    def one(fcId: ModelComponentId): Option[String] =
      if (formModel.isDefinedAt(fcId)) {
        recData.variadicFormData.one(fcId)
      } else None

    def many(fcId: ModelComponentId): Option[Seq[String]] =
      if (formModel.isDefinedAt(fcId)) {
        recData.variadicFormData.many(fcId)
      } else None

    def get(fcId: ModelComponentId): Option[VariadicValue] =
      if (formModel.isDefinedAt(fcId)) {
        recData.variadicFormData.get(fcId)
      } else None
  }

}
