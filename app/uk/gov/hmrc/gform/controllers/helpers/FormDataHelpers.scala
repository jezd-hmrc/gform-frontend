/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers.helpers

import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.show._
import com.softwaremill.quicklens._
import play.api.mvc.Results._
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData.listVariadicFormComponentIds
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormField, FormId, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplate, Group }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

object FormDataHelpers {

  //TODO: fix the bug:
  //for choice component, in mongo we have '1,2,3' but in request from browser we have List(1,2,2)
  //however we can't split formField.value by comma because other data could have it in it
  def formDataMap(formData: FormData): Map[FormComponentId, Seq[String]] =
    formData.fields.map(formField => formField.id -> List(formField.value)).toMap

  def processResponseDataFromBody(request: Request[AnyContent], template: FormTemplate)(
    continuation: VariadicFormData => Future[Result])(implicit hc: HeaderCarrier): Future[Result] =
    request.body.asFormUrlEncoded
      .map(_.map { case (a, b) => (FormComponentId(a), b.map(_.trim)) }) match {
      case Some(data) => continuation(buildVariadicFormDataFromBrowserPostData(template, data))
      case None =>
        Future.successful(BadRequest("Cannot parse body as FormUrlEncoded")) // Thank you play-authorised-frontend for forcing me to do this check
    }

  def get(data: Map[FormComponentId, Seq[String]], id: FormComponentId): List[String] =
    data.get(id).toList.flatten

  def anyFormId(data: Map[FormComponentId, Seq[String]]): Option[FormId] =
    data.get(FormComponentId("formId")).flatMap(_.filterNot(_.isEmpty()).headOption).map(FormId.apply)

  def dataEnteredInGroup(group: Group, fieldData: VariadicFormData): Boolean =
    group.fields
      .map(_.id)
      .exists(id => fieldData.get(id).exists(_.exists(!_.isEmpty)))

  def updateFormField(form: Form, updatedFormField: FormField): Form = {
    val updated: Seq[FormField] = form.formData.fields.filterNot(_.id === updatedFormField.id).+:(updatedFormField)
    form.modify(_.formData.fields).setTo(updated)
  }

  // The VariadicFormData instance returned contains ALL fields in the data map, even if
  // there is no corresponding FormComponentId in the given template.
  // The only use of formTemplate is to determine which branch of VariadicValue each FormComponentId should use,
  // with the assumption that a value of any FormComponentId found in the data map that is not
  // in the formTemplate should be represented by a VariadicValue.One value.
  private def buildVariadicFormDataFromBrowserPostData(
    template: FormTemplate,
    data: Map[FormComponentId, Seq[String]]): VariadicFormData = {
    val variadicFormComponentIds =
      listVariadicFormComponentIds(template.listBasicFormComponents) + VisitIndex.formComponentId

    VariadicFormData(
      data
        .map {
          case (id, s) => (id, variadicFormComponentIds(id.reduceToTemplateFieldId), s.toList)
        }
        .map {
          case (id, true, s)           => (id, VariadicValue.Many(s.filterNot(_.isEmpty)))
          case (id, false, first :: _) => (id, VariadicValue.One(first))
          case (id, false, _) =>
            throw new IllegalArgumentException(
              show"""Got a single value form component ID "$id", with an empty list of values""")
        }
        .toMap
    )
  }
}
