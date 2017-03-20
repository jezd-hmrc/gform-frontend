/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models.helpers

import uk.gov.hmrc.bforms.models._

object Fields {


    def okValues(formFieldMap: Map[FieldId, Seq[String]], fieldValues: List[FieldValue])
                (fieldValue: FieldValue) : Option[FormFieldValidationResult] = {
      val formFields = toFormField(formFieldMap, fieldValues).map(hf => hf.id -> hf).toMap
    fieldValue.`type` match {
      case Address | Date =>
        val fieldOkData =
          formFields.filter {
            case (fieldId, formField) => fieldId.value.startsWith(fieldValue.id.value) // Get just fieldIds related to fieldValue
          }.map {
            case (fieldId, formField) => fieldId.value.replace(fieldValue.id + ".", "") -> FieldOk(fieldValue, formField.value)
          }
        Some(ComponentField(fieldValue, fieldOkData))
      case _ => formFields.get(fieldValue.id).map { formField =>
        FieldOk(fieldValue, formField.value)
      }
    }
  }

  def toFormField(formFields: Map[FieldId, Seq[String]], fieldValue: List[FieldValue]) : List[FormField] = {

    val getFormFieldValue: FieldId => FormField = fieldId => {
      val value = formFields.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    fieldValue.flatMap { fv =>
      fv.`type` match {
        case Address => Address.fields(fv.id).map(getFormFieldValue)
        case Date => Date.fields(fv.id).map(getFormFieldValue)
        case _ => List(getFormFieldValue(fv.id))
      }
    }
  }


}
