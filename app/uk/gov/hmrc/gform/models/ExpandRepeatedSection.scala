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

import cats.Id
import cats.syntax.show._
import scala.util.Try
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalDefault
import uk.gov.hmrc.gform.gform.FormComponentUpdater
import uk.gov.hmrc.gform.graph.{ Convertible, Evaluator }
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.RepeatingPage
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import uk.gov.hmrc.http.HeaderCarrier

object ExpandRepeatedSection {

  def generateDynamicPages[S <: SourceOrigin](
    page: Page[GroupExpanded],
    repeatingPage: Section.RepeatingPage,
    data: VariadicFormData[S],
    formModel: FormModel[GroupExpanded, S],
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    thirdPartyData: ThirdPartyData,
    envelopeId: EnvelopeId
  )(
    implicit hc: HeaderCarrier
  ): List[Page[FullyExpanded]] = {

    val idEvaluator: Evaluator[Id] = new Evaluator[Id]((_, _, template, _) =>
      throw new Exception(show"Cannot do eeitt prepop here! FormTemplate is ${template._id}"))

    /* def eval(
     * visSet: Set[GraphNode],
     * fcId: FormComponentId,
     * expr: Expr,
     * dataLookup: VariadicFormData,
     * retrievals: MaterialisedRetrievals,
     * formTemplate: FormTemplate,
     * thirdPartyData: ThirdPartyData,
     * envelopeId: EnvelopeId) */

    val count: Int = ??? // This needs to be determined based on VariadicFormData[S]
    /* idEvaluator
     * .evalAsString(
     *   data,
     *   FormComponentId("dummy"),
     *   repeatingPage.repeats.expr,
     *   retrievals,
     *   formTemplate,
     *   thirdPartyData,
     *   envelopeId
     * )
     * .flatMap(r => Try(r.toInt).toOption)
     * .getOrElse(1) */

    (1 to count).map { i =>
      copyPage(page, i)
    }.toList

    //List.empty[Page[FullyExpanded]]
  }

  /* private def getRequestedCount2(
   *   expr: TextExpression,
   *   formModel: FormModel[GroupExpanded],
   *   data: FormDataRecalculated): Int = {
   *
   *   val repeatingGroupsFound = findRepeatingGroupsContainingField(expr, formTemplate)
   *
   *   if (repeatingGroupsFound.isEmpty) {
   *     evaluateExpression(expr.expr, formTemplate, data)
   *   } else {
   *     val groupFieldValue: FormComponent = repeatingGroupsFound.head
   *
   *     groupFieldValue match {
   *       case IsGroup(group) =>
   *         val groups: List[GroupList] = ExpandUtils.getAllFieldsInGroup(groupFieldValue, group, data)
   *         groups.map(_.componentList.size).sum
   *       case _ => 0
   *     }
   *   }
   * } */

  /* def generateDynamicSections(
   *   section: Section.RepeatingPage,
   *   formTemplate: FormTemplate,
   *   data: FormDataRecalculated): List[Singleton[FullyExpanded]] = {
   *
   *   val count = getRequestedCount(section.repeats, formTemplate, data)
   *
   *   (1 to count).map { i =>
   *     copySection(section, i, data)
   *   }.toList
   *
   * } */

  private def copyPage(page: Page[GroupExpanded], index: Int): Page[FullyExpanded] = {
    def copyField(field: FormComponent): FormComponent = {
      val tpe = field.`type` match {
        case rc: RevealingChoice =>
          val optionsUpd = rc.options.map(rce => rce.copy(revealingFields = rce.revealingFields.map(copyField)))
          rc.copy(options = optionsUpd)
        case grp @ Group(fields, _, _, _, _, _) =>
          grp.copy(fields = fields.map(copyField))
        case t => t
      }
      FormComponentUpdater(
        field.copy(
          id = FormComponentId(s"${index}_${field.id.value}"),
          `type` = tpe
        ),
        index,
        page
      ).updated
    }

    page.copy(
      title = buildText(page.title, index),
      shortName = optBuildText(page.shortName, index),
      fields = page.fields.map(copyField)
    )

  }

  private def optBuildText(maybeLs: Option[SmartString], index: Int): Option[SmartString] =
    maybeLs.map(ls => buildText(ls, index))

  private def buildText(ls: SmartString, index: Int): SmartString =
    ls.replace("$n", index.toString)

  /* private def sumFunctionality(field: FormCtx, formTemplate: FormTemplate, data: FormDataRecalculated): BigDecimal = {
   *   val repeatFormComponentIds =
   *     RepeatingComponentService.getRepeatFormComponentIds(formTemplate.expandFormTemplate(data.data).allFormComponents)
   *   val fcIds: List[FormComponentId] = repeatFormComponentIds.op(FormComponentId(field.value))
   *   fcIds.map(id => data.data.one(id).fold(0: BigDecimal)(toBigDecimalDefault)).sum
   * } */

  /* //This Evaluation is for the repeating sections, this will not become values.
   *   private def evaluateExpression(expr: Expr, formTemplate: FormTemplate, data: FormDataRecalculated): Int = {
   *     def eval(expr: Expr): Int = expr match {
   *       case Add(expr1, expr2)         => eval(expr1) + eval(expr2)
   *       case Multiply(expr1, expr2)    => eval(expr1) * eval(expr2)
   *       case Subtraction(expr1, expr2) => eval(expr1) - eval(expr2)
   *       case Sum(ctx @ FormCtx(_))     => sumFunctionality(ctx, formTemplate, data).toInt
   *       case formExpr @ FormCtx(_)     => getFormFieldIntValue(TextExpression(formExpr), data)
   *       case Constant(value)           => Try(value.toInt).toOption.getOrElse(0)
   *       // case AuthCtx(value: AuthInfo) =>
   *       // case EeittCtx(value: Eeitt) =>
   *       case _ => 0
   *     }
   *     eval(expr)
   *   } */

  /**
    * This method decide if section is expanded based on repeated group or simple numeric expression
 **/
  /* private def getRequestedCount(expr: TextExpression, formTemplate: FormTemplate, data: FormDataRecalculated): Int = {
   *
   *   val repeatingGroupsFound = findRepeatingGroupsContainingField(expr, formTemplate)
   *
   *   if (repeatingGroupsFound.isEmpty) {
   *     evaluateExpression(expr.expr, formTemplate, data)
   *   } else {
   *     val groupFieldValue: FormComponent = repeatingGroupsFound.head
   *
   *     groupFieldValue match {
   *       case IsGroup(group) =>
   *         val groups: List[GroupList] = ExpandUtils.getAllFieldsInGroup(groupFieldValue, group, data)
   *         groups.map(_.componentList.size).sum
   *       case _ => 0
   *     }
   *   }
   * } */

  /* private def getFormFieldIntValue(expr: TextExpression, data: FormDataRecalculated): Int = {
   *   val id = extractFieldId(expr)
   *
   *   data.data
   *     .one(FormComponentId(id))
   *     .flatMap { v =>
   *       Try(v.toInt).toOption
   *     }
   *     .getOrElse(0)
   * } */

  /* private def extractFieldId(expr: TextExpression) =
   *   expr.expr match {
   *     case FormCtx(fieldId) => fieldId
   *     case _                => ""
   *   } */

  /* private def findRepeatingGroupsContainingField(
 *   expr: TextExpression,
 *   formTemplate: FormTemplate): Set[FormComponent] = {
 *
 *   val id = extractFieldId(expr)
 *
 *   def findRepeatingGroups(groupField: Option[FormComponent], fieldList: List[FormComponent]): Set[FormComponent] =
 *     fieldList.flatMap { field =>
 *       field.`type` match {
 *         case Group(fields, _, repMax, _, _, _) if repMax.isDefined          => findRepeatingGroups(Some(field), fields)
 *         case othertype if groupField.isDefined && field.id.value.equals(id) => List(groupField.get)
 *         case _                                                              => Nil
 *       }
 *     }.toSet
 *
 *   //formTemplate.sections.flatMap(section => findRepeatingGroups(None, section.fields)).toSet
 *   Set.empty
 * } */
}
