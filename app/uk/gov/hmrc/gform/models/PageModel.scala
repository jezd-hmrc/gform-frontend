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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FullyExpanded, IncludeIf, Page, PageMode, Section }

sealed trait PageModel[A <: PageMode] extends Product with Serializable {
  def title: SmartString = this match {
    case Singleton(page, _)          => page.title
    case Repeater(_, title, _, _, _) => title
  }
  def shortName: Option[SmartString] = this match {
    case Singleton(page, _)              => page.shortName
    case Repeater(_, _, shortName, _, _) => shortName
  }

  def isTerminationPage = this match {
    case Singleton(page, _)      => page.isTerminationPage
    case Repeater(_, _, _, _, _) => false
  }

  def expand(formDataRecalculated: FormDataRecalculated): PageModel[FullyExpanded] = ???
  //def expandSectionRc(formDataRecalculated: FormDataRecalculated): ExpandedSection = ???

  def jsFormComponentModels: List[JsFormComponentModel] = this match {
    case Singleton(page, _)      => page.fields.flatMap(_.jsFormComponentModels)
    case Repeater(_, _, _, _, _) => Nil
  }

  def fold[B](f: Singleton[A] => B)(g: Repeater[A] => B): B = this match {
    case s: Singleton[A] => f(s)
    case r: Repeater[A]  => g(r)
  }

  def allFormComponents: List[FormComponent] = ???

}

case class AddToListRecord(value: String) extends AnyVal

case class Singleton[A <: PageMode](page: Page[A], source: Section) extends PageModel[A]
case class Repeater[A <: PageMode](
  records: List[AddToListRecord],
  repTitle: SmartString,
  repShortName: Option[SmartString],
  includeIf: Option[IncludeIf],
  source: Section.AddToList
) extends PageModel[A]
