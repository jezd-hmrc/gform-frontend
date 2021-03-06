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

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LocalisedString, VariadicFormData, formtemplate }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

case class ExpandedFormTemplate(expandedSection: List[ExpandedSection]) {
  val allFormComponents: List[FormComponent] =
    expandedSection.flatMap(_.expandedFormComponents.flatMap(_.formComponents))
  val allFormComponentIds: List[FormComponentId] = expandedSection.flatMap(_.expandedFormComponents.flatMap(_.allIds))
  def formComponentsLookup(data: VariadicFormData): Map[FormComponentId, FormComponent] =
    allFormComponents.flatMap(fc => fc.expandFormComponent(data).allIds.map(_ -> fc)).toMap
  def formComponentsLookupFull: Map[FormComponentId, FormComponent] =
    allFormComponents.flatMap(fc => fc.expandFormComponentFull.allIds.map(_ -> fc)).toMap
  val allIncludeIfs: List[(List[ExpandedFormComponent], IncludeIf, Int)] = expandedSection.zipWithIndex.collect {
    case (ExpandedSection(expandedFormComponents, Some(includeIf)), index) => (expandedFormComponents, includeIf, index)
  }
}

case class FormTemplate(
  _id: FormTemplateId,
  formName: LocalisedString,
  developmentPhase: Option[DevelopmentPhase],
  formCategory: FormCategory,
  draftRetrievalMethod: DraftRetrievalMethod,
  destinations: Destinations,
  authConfig: formtemplate.AuthConfig,
  emailTemplateId: String,
  emailParameters: Option[NonEmptyList[EmailParameter]],
  webChat: Option[WebChat],
  sections: List[Section],
  parentFormSubmissionRefs: List[FormComponentId],
  languages: AvailableLanguages,
  save4LaterInfoText: Option[Save4LaterInfoText],
  summarySection: SummarySection
) {

  val isSpecimen: Boolean = _id.value.startsWith("specimen-")

  def listAllSections: List[BaseSection] =
    destinations match {
      case destinationList: DestinationList =>
        sections ::: List(destinationList.declarationSection, destinationList.acknowledgementSection)
      case _ =>
        sections
    }

  def expandFormTemplate(data: VariadicFormData): ExpandedFormTemplate =
    ExpandedFormTemplate(sections.map(_.expandSection(data)))

  val expandFormTemplateFull: ExpandedFormTemplate = ExpandedFormTemplate(sections.map(_.expandSectionFull))
}

object FormTemplate {

  import JsonUtils._

  private val reads = Reads[FormTemplate] { json =>
    Json.reads[FormTemplate].reads(json)
  }

  implicit val format: OFormat[FormTemplate] = OFormat(reads, derived.owrites[FormTemplate])
}
