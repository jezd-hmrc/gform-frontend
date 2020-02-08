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
import uk.gov.hmrc.gform.models.{ FormModel, PageModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode, SimpleGN }

object JavascriptMaker {

  def generateJs(
    sectionNumber: SectionNumber,
    formModel: FormModel[FullyExpanded],
    formTemplate: FormTemplate): String = {
    val pageModel = formModel(sectionNumber)
    val jsFormComponentModels = pageModel.jsFormComponentModels
    val allAtomicFields = formModel.expandFull.allFormComponents //flatMap(RepeatingComponentService.atomicFieldsFull)

    createJavascript(pageModel, jsFormComponentModels, allAtomicFields, mkDependencies(formTemplate))

  }

  private def mkDependencies(formTemplate: FormTemplate): Dependencies = {
    val graph = DependencyGraph.toGraphFull(formTemplate)

    val graphTopologicalOrder: Either[graph.NodeT, Traversable[(Int, List[GraphNode])]] =
      DependencyGraph.constructDependencyGraph(graph)

    graphTopologicalOrder match {
      case Left(_) => Dependencies(List.empty[FormComponentIdDeps])
      case Right(lto) =>
        val depLayers: Traversable[List[FormComponentId]] =
          lto.map(_._2).map(_.collect { case SimpleGN(fcId) => fcId })
        val (deps, _) =
          depLayers
            .foldRight((List.empty[FormComponentIdDeps], List.empty[FormComponentId])) {
              case (layer, (deps, acc)) =>
                val newDeps = layer.map { fcId =>
                  FormComponentIdDeps(fcId, acc) // all of acc depends on fcId
                }
                (deps ++ newDeps, acc ++ layer)
            }
        Dependencies(deps)
    }
  }

  private def createJavascript(
    pageModel: PageModel[FullyExpanded],
    jsFormComponentModels: List[JsFormComponentModel],
    allAtomicFields: List[FormComponent],
    dependencies: Dependencies): String = {
    val groups: List[(FormComponentId, Group)] = pageModel
      .fold(_.page.fields)(_ => Nil)
      .filter(_.presentationHint.getOrElse(Nil).contains(CollapseGroupUnderLabel))
      .collect {
        case fc @ IsGroup(group) => (fc.id, group)
      }

    // includeIf-CollapsingGroupWithSum.json
    val repeatFormComponentIds = RepeatingComponentService.getRepeatFormComponentIds(allAtomicFields)

    groups.map((Javascript.collapsingGroupJavascript _).tupled).mkString("\n") +
      Javascript.fieldJavascript(jsFormComponentModels, allAtomicFields, repeatFormComponentIds, dependencies)
  }
}
