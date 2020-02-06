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

package uk.gov.hmrc.gform.models.javascript

import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, PageModel }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode

object JavascriptMaker {

  def generateJs(
    sectionNumber: SectionNumber,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formTemplate: FormTemplate): String = {
    val formModel = formModelOptics.formModelRenderPageOptics.formModel
    val pageModel = formModel(sectionNumber)
    val jsFormComponentModels = pageModel.jsFormComponentModels
    //val allAtomicFields = dynamicSections.flatMap(RepeatingComponentService.atomicFieldsFull)
    val allAtomicFields = formModel.allFormComponents // TODO JoVL this used to be full expansion, why?

    createJavascript(pageModel, jsFormComponentModels, allAtomicFields, mkDependencies())

  }

  // TODO JoVl reimplement this
  private def mkDependencies(): Dependencies = Dependencies(List.empty) /* {
   * //val graph = DependencyGraph.toGraph(formModel.asInstanceOf[FormModel[DataExpanded, SourceOrigin.OutOfDate]])
   *
   * /\* val graphTopologicalOrder: Either[graph.NodeT, Traversable[(Int, List[GraphNode])]] =
   *  *   DependencyGraph.constructDependencyGraph(graph) *\/
   *
   * val depLayers: Traversable[List[FormComponentId]] =
   *   graphTopologicalOrder.map(_._2).map(_.collect { case GraphNode.Simple(fcId) => fcId })
   * val (deps, _) =
   *   depLayers
   *     .foldRight((List.empty[FormComponentIdDeps], List.empty[FormComponentId])) {
   *       case (layer, (deps, acc)) =>
   *         val newDeps = layer.map { fcId =>
   *           FormComponentIdDeps(fcId, acc) // all of acc depends on fcId
   *         }
   *         (deps ++ newDeps, acc ++ layer)
   *     }
   * Dependencies(deps) } */

  private def createJavascript(
    pageModel: PageModel[DataExpanded],
    jsFormComponentModels: List[JsFormComponentModel],
    allAtomicFields: List[FormComponent],
    dependencies: Dependencies): String = {
    val groups: List[(FormComponentId, Group)] = pageModel
      .fold(_.page.fields)(_ => Nil)
      //.filter(_.presentationHint.getOrElse(Nil).contains(CollapseGroupUnderLabel))
      .collect {
        case fc @ IsGroup(group) => (fc.id, group)
      }

    // includeIf-CollapsingGroupWithSum.json
    val repeatFormComponentIds = RepeatingComponentService.getRepeatFormComponentIds(allAtomicFields)

    Javascript.fieldJavascript(jsFormComponentModels, allAtomicFields, repeatFormComponentIds, dependencies)
  }
}
