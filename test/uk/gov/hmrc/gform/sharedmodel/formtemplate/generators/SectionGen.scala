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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import org.scalacheck.Gen
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import SmartStringGen.smartStringGen

trait SectionGen {

  def verifierRecipeGen: Gen[VerifierRecipe] =
    for {
      key     <- PrimitiveGen.nonEmptyAlphaNumStrGen
      formCtx <- ExprGen.formCtxGen
    } yield VerifierRecipe(key, formCtx)

  def identifierRecipeGen: Gen[IdentifierRecipe] =
    for {
      key     <- PrimitiveGen.nonEmptyAlphaNumStrGen
      formCtx <- ExprGen.formCtxGen
    } yield IdentifierRecipe(key, formCtx)

  def enrolmentSectionGen: Gen[EnrolmentSection] =
    for {
      title       <- smartStringGen
      shortName   <- Gen.option(smartStringGen)
      fields      <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      identifiers <- PrimitiveGen.oneOrMoreGen(identifierRecipeGen)
      verifiers   <- PrimitiveGen.zeroOrMoreGen(verifierRecipeGen)
    } yield EnrolmentSection(title, shortName, fields.toList, identifiers, verifiers)

  def acknowledgementSectionGen: Gen[AcknowledgementSection] =
    for {
      title         <- smartStringGen
      description   <- Gen.option(smartStringGen)
      shortName     <- Gen.option(smartStringGen)
      fields        <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      showReference <- PrimitiveGen.booleanGen
    } yield AcknowledgementSection(title, description, shortName, fields.toList, showReference)

  def declarationSectionGen: Gen[DeclarationSection] =
    for {
      title       <- smartStringGen
      description <- Gen.option(smartStringGen)
      shortName   <- Gen.option(smartStringGen)
      fields      <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
    } yield DeclarationSection(title, description, shortName, fields.toList)

  def pageGen: Gen[Page[Basic]] =
    for {
      title             <- smartStringGen
      description       <- Gen.option(smartStringGen)
      progressIndicator <- Gen.option(smartStringGen)
      shortName         <- Gen.option(smartStringGen)
      includeIf         <- Gen.option(IncludeIfGen.includeIfGen)
      validators        <- Gen.option(ValidatorGen.validatorGen)
      fields            <- PrimitiveGen.oneOrMoreGen(FormComponentGen.formComponentGen())
      continueLabel     <- Gen.option(smartStringGen)
      continueIf        <- Gen.option(ContinueIfGen.continueIfGen)
    } yield
      Page(
        title,
        description,
        progressIndicator,
        shortName,
        includeIf,
        validators,
        fields.toList,
        continueLabel,
        continueIf
      )

  def nonRepeatingPageSectionGen: Gen[Section.NonRepeatingPage] = pageGen.map(Section.NonRepeatingPage)

  def repeatingPageSectionGen: Gen[Section.RepeatingPage] =
    for {
      page    <- pageGen
      repeats <- ExprGen.exprGen()
    } yield Section.RepeatingPage(page, repeats)

  def addToListSectionGen: Gen[Section.AddToList] =
    for {
      title         <- smartStringGen
      description   <- smartStringGen
      shortName     <- smartStringGen
      includeIf     <- Gen.option(IncludeIfGen.includeIfGen)
      repeatsMax    <- Gen.option(ExprGen.exprGen())
      pages         <- PrimitiveGen.oneOrMoreGen(pageGen)
      formComponent <- FormComponentGen.formComponentGen(0)
      choice        <- ComponentTypeGen.choiceGen
    } yield
      Section
        .AddToList(title, description, shortName, includeIf, repeatsMax, pages, formComponent.copy(`type` = choice))

  def sectionGen: Gen[Section] = Gen.oneOf(nonRepeatingPageSectionGen, repeatingPageSectionGen)
}

object SectionGen extends SectionGen
