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

package uk.gov.hmrc.gform.gform

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.{ AddToListUtils, ExpandUtils }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class FormComponentUpdater(formComponent: FormComponent, index: Int, baseIds: List[FormComponentId]) {

  private def expandExpr(expr: Expr): Expr = ExprUpdater(expr, index, baseIds).updated

  private def expandBooleanExpr(expr: BooleanExpr): BooleanExpr = expr match {
    case Equals(left, right)              => Equals(expandExpr(left), expandExpr(right))
    case NotEquals(left, right)           => NotEquals(expandExpr(left), expandExpr(right))
    case GreaterThan(left, right)         => GreaterThan(expandExpr(left), expandExpr(right))
    case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(expandExpr(left), expandExpr(right))
    case LessThan(left, right)            => LessThan(expandExpr(left), expandExpr(right))
    case LessThanOrEquals(left, right)    => LessThanOrEquals(expandExpr(left), expandExpr(right))
    case Not(e)                           => Not(expandBooleanExpr(e))
    case Or(left, right)                  => Or(expandBooleanExpr(left), expandBooleanExpr(right))
    case And(left, right)                 => And(expandBooleanExpr(left), expandBooleanExpr(right))
    case otherwise                        => otherwise
  }

  val updated = formComponent.copy(
    validIf = formComponent.validIf.map(vi => ValidIf(expandBooleanExpr(vi.expr))),
    `type` = formComponent.`type` match {
      case t: Text          => t.copy(value = expandExpr(t.value))
      case t: TextArea      => t.copy(value = expandExpr(t.value))
      case t: UkSortCode    => t.copy(value = expandExpr(t.value))
      case t: HmrcTaxPeriod => t.copy(idNumber = expandExpr(t.idNumber))
      case otherwise        => otherwise
    },
    label = AddToListUtils.expandSmartString(formComponent.label, index, baseIds),
    validators = formComponent.validators.map { fcv =>
      fcv.copy(validIf = ValidIf(expandBooleanExpr(fcv.validIf.expr)))
    }
  )

  val updatedWithId = updated.copy(id = ExpandUtils.addPrefix(index, formComponent.id))

}

object FormComponentUpdater {
  def apply(formComponent: FormComponent, index: Int, group: Group) =
    new FormComponentUpdater(formComponent, index, group.fields.map(_.id))

  def apply(formComponent: FormComponent, index: Int, page: Page[GroupExpanded]) =
    new FormComponentUpdater(formComponent, index, page.fields.map(_.id))
}
