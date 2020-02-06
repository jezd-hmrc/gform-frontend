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

package uk.gov.hmrc.gform.models.helpers

import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, PageModel, Singleton }
import uk.gov.hmrc.gform.models.ids.{ ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormField, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.sharedmodel.VariadicFormData

object Fields {

  def getHiddenTemplateFields(
    pageModel: PageModel[DataExpanded],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    lookupExtractors: LookupExtractors
  ): RenderHiddenList = {
    val allFormComponents: List[FormComponent] =
      formModelOptics.formModelRenderPageOptics.allFormComponentsExceptFromPage(pageModel)

    val submitted = submittedFCs(formModelOptics, allFormComponents)
    val alwaysEmptyHidden = getAlwaysEmptyHidden(pageModel, lookupExtractors)

    val valueHidden = RenderHidden.ValueHidden(submitted)
    val emptyHidden = RenderHidden.EmptyHidden(alwaysEmptyHidden)

    RenderHiddenList(valueHidden :: emptyHidden :: Nil)
  }

  def flattenGroups(fields: List[FormComponent]): List[FormComponent] =
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => flattenGroups(grp.fields)
        case _          => List(fieldValue)
      }
    }
}
