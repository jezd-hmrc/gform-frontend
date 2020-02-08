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

import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FullyExpanded, Section }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.NonRepeatingPage

trait FormModelSupport {
  def getSingleton(section: Section.NonRepeatingPage): Singleton[FullyExpanded] =
    getSingleton(section, FormDataRecalculated.empty)

  def getSingleton(section: Section.NonRepeatingPage, data: FormDataRecalculated): Singleton[FullyExpanded] = {
    val formTemplate: FormTemplate = mkFormTemplate(section)
    val formModel: FormModel[FullyExpanded] = FormModel.expand(formTemplate, data)
    val singletons: Option[Singleton[FullyExpanded]] = formModel.pages.collectFirst {
      case s: Singleton[_] => s
    }
    singletons.getOrElse(throw new Exception("Wrong test data setup"))
  }

  def getFormModel(sections: List[Section]): FormModel[FullyExpanded] =
    getFormModel(sections, FormDataRecalculated.empty)

  def getFormModel(sections: List[Section], data: FormDataRecalculated): FormModel[FullyExpanded] = {
    val formTemplate: FormTemplate = mkFormTemplate(sections)
    FormModel.expand(formTemplate, data)
  }
}
