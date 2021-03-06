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

package uk.gov.hmrc.gform.graph

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }
import uk.gov.hmrc.gform.sharedmodel.AvailableLanguages
import uk.gov.hmrc.gform.sharedmodel.ExampleData._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

object FormTemplateBuilder {

  def mkGroup(max: Int, formComponents: List[FormComponent]): Group =
    Group(
      formComponents,
      Some(max),
      None,
      None,
      None
    )

  def mkSection(formComponents: FormComponent*): Section = mkSection(formComponents.toList)
  def mkSection(formComponents: List[FormComponent]) =
    Section.NonRepeatingPage(
      Page(
        toSmartString("Section Name"),
        None,
        None,
        None,
        None,
        None,
        formComponents,
        None,
        None
      ))

  def mkSectionIncludeIf(formComponents: List[FormComponent], includeIf: IncludeIf) =
    Section.NonRepeatingPage(
      Page(
        toSmartString("Section Name"),
        None,
        None,
        None,
        Some(includeIf),
        None,
        formComponents,
        None,
        None
      ))

  val ls = toSmartString("Label")

  def mkFormComponent(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId),
      ct,
      ls,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponentEditable(fcId: String, ct: ComponentType) =
    FormComponent(
      FormComponentId(fcId),
      ct,
      ls,
      None,
      None,
      None,
      true,
      true,
      true,
      false,
      false,
      None,
      None
    )

  def mkFormComponent(fcId: String, expr: Expr): FormComponent =
    mkFormComponent(fcId, Text(BasicText, expr))

  def mkFormComponentEditable(fcId: String, expr: Expr): FormComponent =
    mkFormComponentEditable(fcId, Text(BasicText, expr))

  def mkFormTemplate(sections: Section*): FormTemplate = mkFormTemplate(sections.toList)
  def mkFormTemplate(sections: List[Section]) = FormTemplate(
    FormTemplateId("tst1"),
    toLocalisedString("Dependecy heavy experiment"),
    Some(BetaBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    DestinationList(
      NonEmptyList.of(
        HmrcDms(
          DestinationId("TestHmrcDmsId"),
          "TestHmrcDmsFormId",
          TextExpression(Constant("TestHmrcDmsCustomerId")),
          "TestHmrcDmsClassificationType",
          "TestHmrcDmsBusinessArea",
          "",
          true,
          true
        )),
      ackSection,
      DeclarationSection(toSmartString("Declaration"), None, None, Nil)
    ),
    HmrcAgentModule(AllowAnyAgentAffinityUser),
    "randd_confirmation_submission",
    Some(NonEmptyList
      .of(EmailParameter("fullNameVariable", FormCtx("fullName")), EmailParameter("emailVariable", FormCtx("email")))),
    None,
    sections,
    Nil,
    AvailableLanguages.default,
    None,
    SummarySection(toSmartString("Title"), toSmartString("Header"), toSmartString("Footer"))
  )

}
