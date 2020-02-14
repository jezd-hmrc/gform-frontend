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

import cats.instances.int._
import cats.syntax.eq._
import scala.util.Try
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FormComponentId }

object AddToListUtils {

  private val NumericPrefix = "^(\\d+)_(.*)".r

  private def hasPrefix(n: Int, fcId: FormComponentId): Boolean =
    fcId.value.startsWith(n.toString + "_")

  def getPrefix(fcId: FormComponentId): Option[Int] = toComponents(fcId).map(_._1)

  def toComponents(fcId: FormComponentId): Option[(Int, String)] =
    fcId.value match {
      case NumericPrefix(prefix, rest) => Try(prefix.toInt).toOption.map((_, rest))
      case _                           => None
    }

  def removeRecord(processData: ProcessData, idx: Int, addToListId: AddToListId): (VariadicFormData, VisitIndex) = {
    val (c, d) = processData.formModel.pages.partition(_.isAddToList(addToListId))

    val (toBeRemoved, b) =
      c.partition(pageModel =>
        pageModel.fold(s => s.page.fields.forall(field => hasPrefix(idx, field.id)))(_.index === idx))

    val (toBeReindex, keepAsIs) = b.partition(pageModel =>
      pageModel.fold(s => s.page.fields.forall(field => getPrefix(field.id).exists(_ > idx)))(_.index > idx))

    val variadicFormData = processData.data.data

    val s: Set[FormComponentId] = toBeReindex.flatMap(_.allFormComponents.map(_.id)).toSet
    val variadicFormDataToModify = variadicFormData.subset(s)

    val variadicFormDataToModified = variadicFormDataToModify.mapKeys { fcId =>
      val components = toComponents(fcId)
      components.map { case (index, rest) => FormComponentId((index - 1) + "_" + rest) }.getOrElse(fcId)
    }

    val toBeRemovedIds: List[FormComponentId] = toBeRemoved.flatMap(_.allFormComponents.map(_.id))

    val res = variadicFormData -- toBeRemovedIds -- variadicFormDataToModify ++ variadicFormDataToModified

    /* println("variadicFormDataToModify: " + (variadicFormDataToModify))
     * println("variadicFormDataToModified: " + (variadicFormDataToModified))
     *
     * println("variadicFormData: " + (variadicFormData))
     * println("res             : " + (res))
     *
     * println("addToListId: " + (addToListId))
     * println("toBeRemoved: ")
     * toBeRemoved.map(_.allFormComponents.map(_.id)).foreach(println)
     * println("keepAsIs: ")
     * keepAsIs.map(_.allFormComponents.map(_.id)).foreach(println)
     * println("toBeReindex: ")
     * toBeReindex.map(_.allFormComponents.map(_.id)).foreach(println) */

    //val newFormModel = formModelBuilder.fromRawData(res)

    (res, processData.visitsIndex)
  }

}
