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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AddToListId

object AddToListUtils {
  def removeRecord(processData: ProcessData, idx: Int, addToListId: AddToListId): ProcessData = {
    val (a, b) = processData.formModel.pages.partition(pageModel => pageModel.fold(_ => false)(_.index === idx))
    1 + 1
    processData
  }

}
