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

import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Basic, ExpandedFormTemplate, FormComponent, FormComponentId, FormTemplate, FullyExpanded, GroupExpanded, IncludeIf, Page, PageMode, Section, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.{ AddToList, NonRepeatingPage, RepeatingPage }

/* sealed trait FormModelMode extends Product with Serializable
 * trait DataDriven extends FormModelMode
 * trait VisibleOnly extends FormModelMode
 * trait Expanded extends FormModelMode */

case class FormModel[A <: PageMode](pages: List[PageModel[A]]) extends AnyVal {
  def apply(sectionNumber: SectionNumber): PageModel[A] = pages(sectionNumber.value)

  def visible(data: FormDataRecalculated): FormModel[A] = FormModel(pages.filter(data.isVisible))

  def visibleWithIndex(data: FormDataRecalculated): List[(PageModel[A], Int)] = pages.zipWithIndex.collect {
    case (section, index) if data.isVisible(section) => (section, index)
  }

  def expand(data: FormDataRecalculated): ExpandedFormTemplate = ???

  def expandFull: ExpandedFormTemplate = ???

  def allFormComponents: List[FormComponent] = ???
  def allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  def allIncludeIfs: List[(List[FormComponent], IncludeIf, Int)] = pages.zipWithIndex.collect {
    case (pm @ HasIncludeIf(includeIf), index) =>
      (pm.fold(_.page.fields)(_ => List.empty), includeIf, index)
  }

}

object HasIncludeIf {
  def unapply(pageModel: PageModel[_ <: PageMode]): Option[IncludeIf] =
    pageModel.fold(_.page.includeIf)(_.includeIf)
}

object FormModel {

  def empty[A <: PageMode]: FormModel[A] = FormModel[A](List.empty[PageModel[A]])

  def fromRawData(rawData: VariadicFormData, formTemplate: FormTemplate) = {
    val data = FormDataRecalculated(Set.empty, RecData.fromData(rawData))
    FormModel.expand(formTemplate, data)
  }

  def fromCache(cache: AuthCacheWithForm) = fromRawData(cache.variadicFormData, cache.formTemplate)

  def expand(formTemplate: FormTemplate, data: FormDataRecalculated): FormModel[FullyExpanded] = {
    val basicFm: FormModel[Basic] = basic(formTemplate)
    val groupsFm: FormModel[GroupExpanded] = expandGroups(basicFm, data)
    val fullyExpandedFm: FormModel[FullyExpanded] = formModel(groupsFm, data)
    fullyExpandedFm
  }

  def basic(formTemplate: FormTemplate): FormModel[Basic] = FormModel[Basic] {
    formTemplate.sections
      .flatMap {
        case s: Section.NonRepeatingPage => List(Singleton[Basic](s.page, s))
        case s: Section.RepeatingPage    => List(Singleton[Basic](s.page, s))
        case s: Section.AddToList =>
          s.pages.map(page => Singleton[Basic](page, s)).toList ++ List(
            Repeater[Basic](List.empty[AddToListRecord], s.title, s.shortName, s.includeIf, s))

      }
  }

  def expandGroup(page: Page[Basic], data: FormDataRecalculated): Page[GroupExpanded] = ???
  def expandRepeatedSection(
    page: Page[GroupExpanded],
    repeatingPage: Section.RepeatingPage,
    data: FormDataRecalculated): List[Page[FullyExpanded]] =
    ExpandRepeatedSection.generateDynamicPages(page, repeatingPage, data)

  def expandGroups(formModel: FormModel[Basic], data: FormDataRecalculated): FormModel[GroupExpanded] =
    FormModel {
      formModel.pages.map {
        case Singleton(page, source) => Singleton(expandGroup(page, data), source)
        case Repeater(records, title, shortName, includeIf, source) =>
          Repeater[GroupExpanded](records, title, shortName, includeIf, source)
      }
    }

  def formModel(formModel: FormModel[GroupExpanded], data: FormDataRecalculated): FormModel[FullyExpanded] =
    FormModel {
      formModel.pages.flatMap {
        case Singleton(page, source) =>
          source match {
            case s: Section.NonRepeatingPage =>
              List(Singleton[FullyExpanded](page.asInstanceOf[Page[FullyExpanded]], source))
            case s: Section.RepeatingPage =>
              expandRepeatedSection(page, s, data).map(Singleton[FullyExpanded](_, source))
            case s: Section.AddToList => List(Singleton[FullyExpanded](page.asInstanceOf[Page[FullyExpanded]], source))
          }

        case Repeater(records, title, shortName, includeIf, source) =>
          List(Repeater[FullyExpanded](records, title, shortName, includeIf, source))
      }
      /* formTemplate.sections
     *   .flatMap {
     *     case s: Section.NonRepeatingPage => List(Singleton(s.page))
     *     case s: Section.RepeatingPage    => ExpandRepeatedSection.generateDynamicSections(s, formTemplate, data)
     *     case s: Section.AddToList =>
     *       s.pages.map(Singleton.apply).toList ++ List(Repeater(List.empty[String], s.title, s.shortName, s.includeIf))
     *
     *   } */
    }
}
