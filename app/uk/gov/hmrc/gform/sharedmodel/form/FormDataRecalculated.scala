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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.syntax.eq._
import com.softwaremill.quicklens._
import play.api.libs.json._
import uk.gov.hmrc.gform.models.{ FormModel, PageModel }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FullyExpanded, PageMode, Section }
import uk.gov.hmrc.gform.sharedmodel.graph.{ GraphNode, IncludeIfGN, SimpleGN }

case class FormDataRecalculated(
  invisible: Set[GraphNode],
  recData: RecData[SourceOrigin.Current],
  formModel: FormModel[FullyExpanded, SourceOrigin.Current]) {

  val data: VariadicFormData[SourceOrigin.Current] = recData.data // ToDo JoVl Rename to recalculatedData

  def isVisible[A <: PageMode](pageModel: PageModel[A]): Boolean =
    !invisible.exists {
      case SimpleGN(_)               => false
      case IncludeIfGN(_, includeIf) => pageModel.fold(_.page.includeIf)(_.includeIf).exists(_ === includeIf)
    }

  /* def isVisibleFormModel(section: FormModel[FullyExpanded, SourceOrigin.Current]): Boolean =
 *   !invisible.exists {
 *     case SimpleGN(_)               => false
 *     case IncludeIfGN(_, includeIf) => section.includeIf.exists(_ === includeIf)
 *   } */
}

object FormDataRecalculated {
  val empty = FormDataRecalculated(Set.empty, RecData.empty, FormModel.empty)

  def clearTaxResponses(data: FormDataRecalculated): FormDataRecalculated =
    data
      .modify(_.recData.data)
      .setTo(data.recData.cleared)
}
