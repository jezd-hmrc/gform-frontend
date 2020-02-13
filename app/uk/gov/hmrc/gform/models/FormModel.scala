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

import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.{ ExprUpdater, FormComponentUpdater }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Expr
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Basic, Exhaustive, ExpandedFormComponent, FormComponent, FormComponentId, FormTemplate, FullyExpanded, GroupExpanded, IncludeIf, Page, PageMode, Section, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.{ AddToList, NonRepeatingPage, RepeatingPage }
import uk.gov.hmrc.http.HeaderCarrier

case class FormModel[A <: PageMode](pages: List[PageModel[A]]) extends AnyVal {
  def apply(sectionNumber: SectionNumber): PageModel[A] = pages(sectionNumber.value)
  def apply(sectionNumber: Int): PageModel[A] = pages(sectionNumber)

  def visible(data: FormDataRecalculated): FormModel[A] = FormModel(pages.filter(data.isVisible))

  def visibleWithIndex(data: FormDataRecalculated): List[(PageModel[A], SectionNumber)] = pages.zipWithIndex.collect {
    case (section, index) if data.isVisible(section) => (section, SectionNumber(index))
  }

  def toLookup: Map[FormComponentId, FormComponent] =
    allFormComponents.map(fc => fc.id -> fc).toMap

  def allFormComponents: List[FormComponent] = pages.flatMap(_.allFormComponents)
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

object FormModelBuilder {
  def fromCache(cache: AuthCacheWithForm): FormModelBuilder =
    new FormModelBuilder(cache.retrievals, cache.formTemplate, cache.form.thirdPartyData, cache.form.envelopeId)
}

class FormModelBuilder(
  retrievals: MaterialisedRetrievals,
  formTemplate: FormTemplate,
  thirdPartyData: ThirdPartyData,
  envelopeId: EnvelopeId
) {

  def fromRawData(
    rawData: VariadicFormData
  )(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded] = {
    val data = FormDataRecalculated(Set.empty, RecData.fromData(rawData))
    expand(data)
  }

  def expand(
    data: FormDataRecalculated
  )(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded] = {
    val basicFm: FormModel[Basic] = basic()
    val groupsFm: FormModel[GroupExpanded] = expandGroups(basicFm, data)
    val fullyExpandedFm: FormModel[FullyExpanded] = mkFormModel(groupsFm, data)
    fullyExpandedFm
  }

  private def mkRepeater(s: Section.AddToList, index: Int): Repeater[Basic] = {
    val fc = new FormComponentUpdater(s.formComponent, index, s.allIds).updatedWithId
    //println("mkRepeater: " + (fc))
    Repeater[Basic](s.title, s.description, s.shortName, s.includeIf, fc, index, s)
  }

  private def mkSingleton(page: Page[Basic], index: Int): Section.AddToList => Page[Basic] = source => {
    //val fc = new FormComponentUpdater(s.formComponent, index, source.allIds).updatedWithId

    page.copy(fields = page.fields.map(field => new FormComponentUpdater(field, index, source.allIds).updatedWithId))
    //Singleton()
    //???
  }

  private def basicAddToList(s: Section.AddToList, index: Int): List[PageModel[Basic]] =
    s.pages.map(page => Singleton[Basic](mkSingleton(page, index)(s), s)).toList ++ List(mkRepeater(s, index))

  def basic(): FormModel[Basic] = FormModel[Basic] {
    formTemplate.sections
      .flatMap {
        case s: Section.NonRepeatingPage => List(Singleton[Basic](s.page, s))
        case s: Section.RepeatingPage    => List(Singleton[Basic](s.page, s))
        case s: Section.AddToList        => basicAddToList(s, 1)
      }
  }

  def expandGroup(page: Page[Basic], data: FormDataRecalculated): Page[GroupExpanded] = {
    val ss: List[ExpandedFormComponent] = page.fields.map(_.expandFormComponent(data.recData.data))
    page.copy(fields = ss.flatMap(_.formComponents))
  }

  def expandRepeatedSection(
    page: Page[GroupExpanded],
    repeatingPage: Section.RepeatingPage,
    data: FormDataRecalculated,
    formModel: FormModel[GroupExpanded]
  )(
    implicit hc: HeaderCarrier
  ): List[Page[FullyExpanded]] =
    ExpandRepeatedSection
      .generateDynamicPages(page, repeatingPage, data, formModel, retrievals, formTemplate, thirdPartyData, envelopeId)

  def expandGroups(formModel: FormModel[Basic], data: FormDataRecalculated): FormModel[GroupExpanded] =
    FormModel {
      formModel.pages.map {
        case Singleton(page, source) => Singleton(expandGroup(page, data), source)
        case Repeater(title, description, shortName, includeIf, formComponent, index, source) =>
          Repeater[GroupExpanded](title, description, shortName, includeIf, formComponent, index, source)
      }
    }

  def mkFormModel(
    formModel: FormModel[GroupExpanded],
    data: FormDataRecalculated
  )(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded] =
    FormModel {
      formModel.pages.flatMap {
        case Singleton(page, source) =>
          source match {
            case s: Section.NonRepeatingPage =>
              List(Singleton[FullyExpanded](page.asInstanceOf[Page[FullyExpanded]], source))
            case s: Section.RepeatingPage =>
              expandRepeatedSection(page, s, data, formModel).map(Singleton[FullyExpanded](_, source))
            case s: Section.AddToList =>
              List(Singleton[FullyExpanded](page.asInstanceOf[Page[FullyExpanded]], source))
          }

        case Repeater(title, description, shortName, includeIf, formComponent, index, source) =>
          val repeater =
            Repeater[FullyExpanded](title, description, shortName, includeIf, formComponent, index, source)
          val nextOne: Option[Seq[String]] = data.recData.data.many(formComponent.id)
          val next = nextOne.toSeq.flatten
          //println("nextOne           : " + (nextOne))
          //println("next              : " + (next))
          //println("next.contains( 0 ): " + (next.contains("0")) + ", " + index)
          val rest = if (next.contains("0")) {

            val abc: FormModel[Basic] = FormModel(basicAddToList(source, index + 1))
            val efg: FormModel[GroupExpanded] = expandGroups(abc, data)
            val hij: FormModel[FullyExpanded] = mkFormModel(efg, data)
            hij.pages
          } else {
            Nil
          }
          repeater :: rest
      }
    }

}

object FormModel {

  def empty[A <: PageMode]: FormModel[A] = FormModel[A](List.empty[PageModel[A]])

  def expandFull(formTemplate: FormTemplate): FormModel[Exhaustive] = ???

  def fromCache(cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded] = FormModelBuilder.fromCache(cache).fromRawData(cache.variadicFormData)

}
