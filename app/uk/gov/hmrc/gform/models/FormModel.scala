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

import cats.MonadError
import cats.syntax.eq._
import cats.instances.int._
import scala.concurrent.Future
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.{ ExprUpdater, FormComponentUpdater }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation }
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, Basic, Exhaustive, ExpandedFormComponent, Expr, FormComponent, FormComponentId, FormTemplate, FullyExpanded, GroupExpanded, IncludeIf, Page, PageMode, Section, SectionNumber }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.{ AddToList, NonRepeatingPage, RepeatingPage }
import uk.gov.hmrc.http.HeaderCarrier

case class FormModel[A <: PageMode, S <: SourceOrigin](pages: List[PageModel[A]]) extends AnyVal {
  def apply(sectionNumber: SectionNumber): PageModel[A] = pages(sectionNumber.value)
  def apply(sectionNumber: Int): PageModel[A] = pages(sectionNumber)

  def visible(data: FormDataRecalculated): FormModel[A, S] = FormModel(pages.filter(data.isVisible))

  def visibleWithIndex(data: FormDataRecalculated): List[(PageModel[A], SectionNumber)] =
    pages.zipWithIndex.collect {
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

  def lastSectionNumberWith(addToListId: AddToListId): SectionNumber =
    SectionNumber(pages.lastIndexWhere(pm => pm.fold(_ => false)(r => r.source.id === addToListId)))

  def firstsAddToList: Map[AddToListId, Int] =
    pages.zipWithIndex.foldRight(Map.empty[AddToListId, Int]) {
      case ((pageModel, index), acc) =>
        pageModel.sourceIsAddToList.fold(acc) { addToList =>
          acc + (addToList.id -> index)
        }
    }

  def addToListCount(addToListId: AddToListId) = pages.foldRight(0) {
    case (page, acc) =>
      page.addToListCount(addToListId) + acc
  }

  def repeaters(addToListId: AddToListId): List[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collect {
      case IsRepeater(repeater) => repeater
    }
  }

  def repeaterFor(index: Int, addToListId: AddToListId): Option[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collectFirst {
      case IsRepeater(repeater) if repeater.index === index => repeater
    }
  }

  def repeaterFor(addToListId: AddToListId): Option[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collectFirst {
      case IsRepeater(repeater) => repeater
    }
  }
}

class IsRepeater(addToListId: AddToListId) {
  def unapply[A <: PageMode](pageModel: PageModel[A]): Option[Repeater[A]] =
    pageModel.repeaterOf(addToListId)
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

  //  def recalculation: Recalculation[Future, Throwable] = ???

  // Expands repeated sections based on data repeatsMax expression
  def fromFormDataRecalculated(
    rawData: VariadicFormData[SourceOrigin.Current]
  )(
    implicit
    hc: HeaderCarrier
  ): FormModel[FullyExpanded, SourceOrigin.Current] = ???
  //val ssss = recalculation.recalculateFormData(rawData, formTemplate, retrievals, thirdPartyData, envelopeId)
  //val data = FormDataRecalculated(Set.empty, RecData.fromData(rawData), FormModel.empty)
  //expand(rawData)

  // Expands repaeted sections based on data in VariadicFormData
  def fromRawData(
    rawData: VariadicFormData[SourceOrigin.OutOfDate]
  )(
    implicit
    hc: HeaderCarrier
  ): FormModel[FullyExpanded, SourceOrigin.OutOfDate] = ???
  //val ssss = recalculation.recalculateFormData(rawData, formTemplate, retrievals, thirdPartyData, envelopeId)
  //val data = FormDataRecalculated(Set.empty, RecData.fromData(rawData), FormModel.empty)
  //expand(rawData)

  def expand[S <: SourceOrigin](
    //data: FormDataRecalculated
    data: VariadicFormData[S]
  )(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded, S] = {
    val basicFm: FormModel[Basic, S] = basic()
    val groupsFm: FormModel[GroupExpanded, S] = expandGroups(basicFm, data)
    val fullyExpandedFm: FormModel[FullyExpanded, S] = mkFormModel(groupsFm, data)
    //println("fullyExpandedFm.pages: ")
    //fullyExpandedFm.pages.foreach(println)
    fullyExpandedFm
  }

  private def mkRepeater(s: Section.AddToList, index: Int): Repeater[Basic] = {
    val fc = new FormComponentUpdater(s.addAnotherQuestion, index, s.allIds).updatedWithId
    //println("mkRepeater: " + (fc))
    Repeater[Basic](s.title, s.description, s.shortName, s.includeIf, fc, index, s)
  }

  private def mkSingleton(page: Page[Basic], index: Int): Section.AddToList => Page[Basic] = source => {
    val expand: SmartString => SmartString = AddToListUtils.expandSmartString(_, index, source)
    page.copy(
      title = expand(page.title),
      description = page.description.map(expand),
      shortName = page.shortName.map(expand),
      progressIndicator = page.progressIndicator.map(expand),
      continueLabel = page.continueLabel.map(expand),
      fields = page.fields.map(field => new FormComponentUpdater(field, index, source.allIds).updatedWithId)
    )
  }

  private def basicAddToList(s: Section.AddToList, index: Int): List[PageModel[Basic]] =
    s.pages.map(page => Singleton[Basic](mkSingleton(page, index)(s), s)).toList ++ List(mkRepeater(s, index))

  private def basic[S <: SourceOrigin](): FormModel[Basic, S] = FormModel[Basic, S] {
    formTemplate.sections
      .flatMap {
        case s: Section.NonRepeatingPage => List(Singleton[Basic](s.page, s))
        case s: Section.RepeatingPage    => List(Singleton[Basic](s.page, s))
        case s: Section.AddToList        => basicAddToList(s, 1)
      }
  }

  private def expandGroup[S <: SourceOrigin](page: Page[Basic], data: VariadicFormData[S]): Page[GroupExpanded] = {
    val ss: List[ExpandedFormComponent] = page.fields.map(_.expandFormComponent(data))
    page.copy(fields = ss.flatMap(_.formComponents))
  }

  private def expandRepeatedSection[S <: SourceOrigin](
    page: Page[GroupExpanded],
    repeatingPage: Section.RepeatingPage,
    data: VariadicFormData[S],
    formModel: FormModel[GroupExpanded, S]
  )(
    implicit hc: HeaderCarrier
  ): List[Page[FullyExpanded]] =
    ExpandRepeatedSection
      .generateDynamicPages(page, repeatingPage, data, formModel, retrievals, formTemplate, thirdPartyData, envelopeId)

  private def expandGroups[S <: SourceOrigin](
    formModel: FormModel[Basic, S],
    data: VariadicFormData[S]): FormModel[GroupExpanded, S] =
    FormModel {
      formModel.pages.map {
        case Singleton(page, source) => Singleton(expandGroup(page, data), source)
        case Repeater(title, description, shortName, includeIf, formComponent, index, source) =>
          Repeater[GroupExpanded](title, description, shortName, includeIf, formComponent, index, source)
      }
    }

  private def mkFormModel[S <: SourceOrigin](
    formModel: FormModel[GroupExpanded, S],
    data: VariadicFormData[S]
  )(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded, S] =
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
          val exTitle = AddToListUtils.expandSmartString(title, index, source)
          val exShortName = AddToListUtils.expandSmartString(shortName, index, source)
          val exDescription = AddToListUtils.expandSmartString(description, index, source)
          val repeater =
            Repeater[FullyExpanded](exTitle, exDescription, exShortName, includeIf, formComponent, index, source)
          val nextOne: Option[Seq[String]] = data.many(formComponent.id)
          val next = nextOne.toSeq.flatten

          val rest = if (next.contains("0")) {
            val abc: FormModel[Basic, S] = FormModel(basicAddToList(source, index + 1))
            val efg: FormModel[GroupExpanded, S] = expandGroups(abc, data)
            val hij: FormModel[FullyExpanded, S] = mkFormModel(efg, data)
            hij.pages
          } else {
            Nil
          }
          repeater :: rest
      }
    }

}

object FormModel {

  def empty[A <: PageMode, S <: SourceOrigin]: FormModel[A, S] = FormModel[A, S](List.empty[PageModel[A]])

  def fromCache(cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier
  ): FormModel[FullyExpanded, SourceOrigin.Current] =
    FormModelBuilder.fromCache(cache).fromRawData(cache.variadicFormData)

}
