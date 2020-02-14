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

import uk.gov.hmrc.gform.models.javascript.JsFormComponentModel
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FormComponent, FullyExpanded, IncludeIf, Page, PageMode, Section }

sealed trait PageModel[A <: PageMode] extends Product with Serializable {
  def title: SmartString = fold(_.page.title)(_.repTitle)

  def shortName: Option[SmartString] = fold(_.page.shortName)(_.repShortName)

  def isTerminationPage = fold(_.page.isTerminationPage)(_ => false)

  def jsFormComponentModels: List[JsFormComponentModel] = fold(_.page.fields.flatMap(_.jsFormComponentModels))(_ => Nil)

  def isAddToList(addToListId: AddToListId) =
    fold(_.source.byAddToListId(addToListId))(_.source.byAddToListId(addToListId))

  def fold[B](f: Singleton[A] => B)(g: Repeater[A] => B): B = this match {
    case s: Singleton[A] => f(s)
    case r: Repeater[A]  => g(r)
  }

  def allFormComponents: List[FormComponent] = fold(_.page.fields)(r => r.formComponent :: Nil)

}

case class AddToListRecord(value: String) extends AnyVal

case class Singleton[A <: PageMode](page: Page[A], source: Section) extends PageModel[A]
case class Repeater[A <: PageMode](
  repTitle: SmartString,
  repDescription: Option[SmartString],
  repShortName: Option[SmartString],
  includeIf: Option[IncludeIf],
  formComponent: FormComponent,
  index: Int,
  source: Section.AddToList
) extends PageModel[A]
