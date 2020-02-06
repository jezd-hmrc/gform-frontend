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

import scala.util.Try
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.models.javascript.{ FormComponentSimple, FormComponentWithCtx, FormComponentWithGroup }

sealed trait ExprCardinality
case class SingleExpr(expr: Expr) extends ExprCardinality
//case class MultipleExpr(exprs: List[Expr]) extends ExprCardinality

object HasExpr {
  def unapply(fc: FormComponent): Option[ExprCardinality] = unapply(fc.`type`)

  def unapply(ct: ComponentType): Option[ExprCardinality] =
    ct match {
      case Text(_, NonValueExpr(expr), _, _)       => Some(SingleExpr(expr))
      case TextArea(_, NonValueExpr(expr), _)      => Some(SingleExpr(expr))
      case UkSortCode(NonValueExpr(expr))          => Some(SingleExpr(expr))
      case HmrcTaxPeriod(_, NonValueExpr(expr), _) => Some(SingleExpr(expr))
      //case Group(fields, _, _, _, _, _) => Some(MultipleExpr(fields))
      case _ => None
    }
}

private object NonValueExpr {
  def unapply(expr: Expr): Option[Expr] =
    expr match {
      case Value => None
      case _     => Some(expr)
    }
}

object HasExprCtx {
  def unapply(fc: FormComponentWithCtx): Option[ExprCardinality] = fc match {
    case FormComponentWithGroup(fc, _) => HasExpr.unapply(fc.`type`)
    case FormComponentSimple(fc)       => HasExpr.unapply(fc.`type`)
  }
}

object IsNumberConstant {
  def unapply(expr: Expr): Option[BigDecimal] = expr match {
    case Constant(c) => BigDecimalUtil.toBigDecimalSafe(c)
    case _           => None
  }
}

object IsWholeNumberConstant {
  def unapply(expr: Expr): Option[Int] = expr match {
    case Constant(c) => Try(c.toInt).toOption
    case _           => None
  }
}
