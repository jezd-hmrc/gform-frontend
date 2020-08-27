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

package uk.gov.hmrc.gform.summary

import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import cats.instances.int._
import cats.syntax.eq._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Call, Request }
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.commons.MarkDownUtil.markDownParser
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadAlgebra }
import uk.gov.hmrc.gform.gform.{ HtmlSanitiser, SummaryPagePurpose }
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.{ Atom, FastForward, FormModel, FormModelBuilder, PageModel, Repeater, SectionSelector, SectionSelectorType, Singleton, Visibility }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.helpers.Fields.flattenGroups
import uk.gov.hmrc.gform.models.helpers.{ Fields, TaxPeriodHelper }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, ValidatorsResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga.sectionTitle4GaFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.PrintSection.PdfNotification
import uk.gov.hmrc.gform.validation.{ FieldOk, FormFieldValidationResult, MultiFieldId, ValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.views.html.summary.snippets
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.views.html.summary.summary
import uk.gov.hmrc.gform.views.summary.SummaryListRowHelper._
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{ SummaryList, SummaryListRow }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.views.html.errorInline
import uk.gov.hmrc.gform.views.summary.TextFormatter._
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.HtmlContent
import uk.gov.hmrc.gform.models.helpers.DateHelperFunctions.{ getMonthValue, renderMonth }
import uk.gov.hmrc.gform.models.helpers.TaxPeriodHelper.formatDate
import uk.gov.hmrc.gform.views.summary.TextFormatter
import uk.gov.hmrc.govukfrontend.views.html.components.{ ErrorMessage, govukErrorMessage, govukSummaryList }

import scala.collection.immutable
import scala.concurrent.{ ExecutionContext, Future }

class SummaryRenderingService(
  i18nSupport: I18nSupport,
  fileUploadAlgebra: FileUploadAlgebra[Future],
  recalculation: Recalculation[Future, Throwable],
  validationService: ValidationService,
  frontendAppConfig: FrontendAppConfig) {

  def createHtmlForPdf[D <: DataOrigin, U <: SectionSelectorType: SectionSelector](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    submissionDetails: Option[SubmissionDetails],
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[D]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] = {
    import i18nSupport._

    for {
      summaryHtml <- getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics)
    } yield {
      val (extraData, declarationExtraData) = addExtraDataToDocument(submissionDetails, cache)
      PdfHtml(
        HtmlSanitiser
          .sanitiseHtmlForPDF(
            summaryHtml,
            document =>
              HtmlSanitiser.acknowledgementPdf(document, extraData, declarationExtraData, cache.formTemplate)))
    }
  }

  def createHtmlForPrintPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdf: PrintSection.Pdf,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] = {
    import i18nSupport._
    for {
      summaryHtml <- getSummaryHTML(maybeAccessCode, cache, summaryPagePurpose, formModelOptics)
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdf.header, pdf.footer)
      PdfHtml(
        HtmlSanitiser
          .sanitiseHtmlForPDF(summaryHtml, document => HtmlSanitiser.printSectionPdf(document, headerStr, footerStr)))
    }
  }

  def createHtmlForNotificationPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfNotification: PdfNotification
  )(
    implicit request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[PdfHtml] = {
    import i18nSupport._

    val pdfFieldIds = pdfNotification.fieldIds
    val pdfHeader = pdfNotification.header
    val pdfFooter = pdfNotification.footer

    for {
      pdfHtml <- getNotificationPdfHTML(
                  cache.form.formTemplateId,
                  maybeAccessCode,
                  cache,
                  summaryPagePurpose,
                  pdfFieldIds)
    } yield {
      val (headerStr, footerStr) = addDataToPrintPdfHTML(pdfHeader, pdfFooter)
      PdfHtml(
        HtmlSanitiser
          .sanitiseHtmlForPDF(pdfHtml, document => HtmlSanitiser.printSectionPdf(document, headerStr, footerStr)))
    }
  }

  private def addExtraDataToDocument[U <: SectionSelectorType: SectionSelector](
    submissionDetails: Option[SubmissionDetails],
    cache: AuthCacheWithForm
  )(
    implicit hc: HeaderCarrier,
    messages: Messages,
    curLang: LangADT,
    lise: SmartStringEvaluator
  ): (String, String) = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = submissionDetails.map(sd =>
      s"""${sd.submission.submittedDate.format(dateFormat)} ${sd.submission.submittedDate.format(timeFormat)}""")

    val rows = List(
      formattedTime.map(ft => cya_row(messages("submission.date"), ft)),
      Some(cya_row(messages("submission.reference"), SubmissionRef(cache.form.envelopeId).toString)),
      submissionDetails.map(sd => cya_row(messages("submission.mark"), sd.hashedValue))
    ).flatten

    val extraData = cya_section(messages("submission.details"), HtmlFormat.fill(rows)).toString()

    val declaration: List[(FormComponent, Seq[String])] = cache.formTemplate.destinations match {
      case destinationList: DestinationList =>
        for {
          formTemplateDecField <- flattenGroups(destinationList.declarationSection.fields)
          formData             <- cache.variadicFormData.get(formTemplateDecField.modelComponentId) // TODO JoVl - support multifield on declaration section
        } yield (formTemplateDecField, formData.toSeq)

      case _ =>
        Nil
    }

    val declarationExtraData = cya_section(
      messages("submission.declaration.details"),
      HtmlFormat.fill(declaration.map {
        case (formDecFields, formData) => cya_row(formDecFields.label.value, formData.mkString)
      })
    ).toString()

    (extraData, declarationExtraData)

  }

  private def addDataToPrintPdfHTML(
    pdfHeader: SmartString,
    pdfFooter: SmartString
  )(
    implicit
    curLang: LangADT,
    lise: SmartStringEvaluator
  ): (String, String) = {

    val headerHtml = markDownParser(pdfHeader).toString
    val footerHtml = markDownParser(pdfFooter).toString

    (headerHtml, footerHtml)

  }

  def getSummaryHTML[D <: DataOrigin](
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    formModelOptics: FormModelOptics[D]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator): Future[Html] = {
    val envelopeF = fileUploadAlgebra.getEnvelope(cache.form.envelopeId)

    import i18nSupport._

    for {
      envelope <- envelopeF
      validationResult <- validationService
                           .validateFormModel(cache.toCacheData, envelope, formModelOptics.formModelVisibilityOptics)
    } yield
      SummaryRenderingService.renderSummary(
        cache.formTemplate,
        validationResult,
        formModelOptics,
        maybeAccessCode,
        envelope,
        cache.retrievals,
        frontendAppConfig,
        cache.form.thirdPartyData.obligations,
        cache.form.thirdPartyData.reviewComments,
        summaryPagePurpose
      )

  }

  def getNotificationPdfHTML(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId]
  )(
    implicit
    request: Request[_],
    l: LangADT,
    hc: HeaderCarrier,
    ec: ExecutionContext,
    lise: SmartStringEvaluator
  ): Future[Html] = ??? /* {
 *   val dataRaw = cache.variadicFormData
 *   val envelopeF = fileUploadAlgebra.getEnvelope(cache.form.envelopeId)
 *
 *   import i18nSupport._
 *
 *   for {
 *     envelope <- envelopeF
 *     (v, _)   <- validationService.validateFormModel(cache, envelope)
 *   } yield
 *     SummaryRenderingService.renderNotificationPdfSummary(
 *       cache.formTemplate,
 *       v,
 *       data,
 *       maybeAccessCode,
 *       envelope,
 *       cache.retrievals,
 *       frontendAppConfig,
 *       cache.form.thirdPartyData.obligations,
 *       cache.form.thirdPartyData.reviewComments,
 *       summaryPagePurpose,
 *       pdfFieldIds
 *     )
 * } */
}

object SummaryRenderingService {

  def renderSummary[D <: DataOrigin](
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    reviewerComments: Option[String],
    summaryPagePurpose: SummaryPagePurpose
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): Html = {
    val headerHtml = markDownParser(formTemplate.summarySection.header)
    val footerHtml = markDownParser(formTemplate.summarySection.footer)

    val envelopeUpd =
      summaryPagePurpose match {
        case SummaryPagePurpose.ForUser => envelope.withUserFileNames
        case SummaryPagePurpose.ForDms  => envelope
      }
    val sfr =
      summaryRowsForRender(
        validationResult,
        formModelOptics,
        maybeAccessCode,
        formTemplate,
        envelopeUpd,
        obligations,
        reviewerComments
      )
    summary(
      formTemplate,
      sfr,
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig,
      summaryPagePurpose,
      reviewerComments,
      headerHtml,
      footerHtml
    )
  }

  /* def renderSummary(
   *   formTemplate: FormTemplate,
   *   validationResult: ValidationResult,
   *   formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
   *   maybeAccessCode: Option[AccessCode],
   *   envelope: Envelope,
   *   retrievals: MaterialisedRetrievals,
   *   frontendAppConfig: FrontendAppConfig,
   *   obligations: Obligations,
   *   reviewerComments: Option[String],
   *   summaryPagePurpose: SummaryPagePurpose
   * )(
   *   implicit
   *   request: Request[_],
   *   messages: Messages,
   *   l: LangADT,
   *   lise: SmartStringEvaluator
   * ): Html = {
   *   val headerHtml = markDownParser(formTemplate.summarySection.header)
   *   val footerHtml = markDownParser(formTemplate.summarySection.footer)
   *
   *   val envelopeUpd =
   *     summaryPagePurpose match {
   *       case SummaryPagePurpose.ForUser => envelope.withUserFileNames
   *       case SummaryPagePurpose.ForDms  => envelope
   *     }
   *   val sfr =
   *     summaryForRender(
   *       validationResult,
   *       formModelVisibilityOptics,
   *       maybeAccessCode,
   *       formTemplate,
   *       envelopeUpd,
   *       obligations,
   *       reviewerComments
   *     )
   *   summary(
   *     formTemplate,
   *     sfr,
   *     maybeAccessCode,
   *     formTemplate.formCategory,
   *     retrievals.renderSaveAndComeBackLater,
   *     retrievals.continueLabelKey,
   *     frontendAppConfig,
   *     summaryPagePurpose,
   *     reviewerComments,
   *     headerHtml,
   *     footerHtml
   *   )
   * } */

  def renderNotificationPdfSummary(
    formTemplate: FormTemplate,
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    frontendAppConfig: FrontendAppConfig,
    obligations: Obligations,
    reviewerComments: Option[String],
    summaryPagePurpose: SummaryPagePurpose,
    pdfFieldIds: List[FormComponentId]
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Html = {
    val headerHtml = markDownParser(formTemplate.summarySection.header)
    val footerHtml = markDownParser(formTemplate.summarySection.footer)
    val sfr =
      summaryForNotificationPdf(
        validationResult,
        formModelVisibilityOptics,
        maybeAccessCode,
        formTemplate,
        envelope,
        obligations,
        reviewerComments,
        pdfFieldIds
      )
    summary(
      formTemplate,
      sfr,
      maybeAccessCode,
      formTemplate.formCategory,
      retrievals.renderSaveAndComeBackLater,
      retrievals.continueLabelKey,
      frontendAppConfig,
      summaryPagePurpose,
      reviewerComments,
      headerHtml,
      footerHtml
    )
  }

  def summaryRowsForRender[D <: DataOrigin](
    validationResult: ValidationResult,
    formModelOptics: FormModelOptics[D],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations,
    reviewerComments: Option[String] = None
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[Html] = {

    val formModel = formModelOptics.formModelVisibilityOptics.formModel

    def renderHtmls(singleton: Singleton[Visibility], sectionNumber: SectionNumber)(implicit l: LangADT): List[Html] = {

      val page = singleton.page

      val sectionTitle4Ga = sectionTitle4GaFactory(page.title, sectionNumber)
      val shortNameOrTitle = page.shortName.getOrElse(page.title)
      val begin = begin_section(shortNameOrTitle)

      val middleRows: List[SummaryListRow] = page.fields
        .filterNot(_.hideOnSummary)
        .flatMap(
          formComponent =>
            getSummaryListRows(
              formComponent,
              formTemplate._id,
              formModelOptics.formModelVisibilityOptics,
              maybeAccessCode,
              sectionNumber,
              sectionTitle4Ga,
              obligations,
              validationResult,
              envelope
          )
        )

      if (middleRows.isEmpty) {
        Nil
      } else {
        val middleRowsHtml = new govukSummaryList()(SummaryList(middleRows, "govuk-!-margin-bottom-9"))
        List(begin, middleRowsHtml)
      }
    }

    def addToListRender(addToList: Section.AddToList): Html = {
      val repeaters: List[Repeater[Visibility]] = formModel.repeaters(addToList.id)
      val sectionNumber = formModelOptics.formModelRenderPageOptics.formModel.lastSectionNumberWith(addToList.id)
      val recordTable: List[SmartString] = repeaters.map(_.expandedDescription)

      val sectionTitle4Ga: SectionTitle4Ga = sectionTitle4GaFactory(addToList.title, sectionNumber)

      val url: Call = routes.FormController
        .form(formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga, SuppressErrors.Yes, FastForward.Yes)

      val value = recordTable.map(_.value).mkString("</br>")

      val slr: SummaryListRow = summaryListRow(
        addToList.title.value,
        value,
        None,
        "",
        "",
        "",
        (url, messages("addToList.addOrRemove")) :: Nil
      )

      new govukSummaryList()(SummaryList(slr :: Nil, "govuk-!-margin-bottom-9"))
    }

    val pagesToRender: List[(PageModel[Visibility], SectionNumber)] = formModel.pagesWithIndex

    pagesToRender.flatMap {
      case (pageModel, sectionNumber) =>
        val firstSingletonOfAddToList = new IsFirstSingletonOfAddToList(sectionNumber, formModel)
        val nextRepeaterAfterRepeater = new NextRepeaterAfterRepeater(formModel)
        pageModel match {
          case firstSingletonOfAddToList(addToList, singleton, repeater) =>
            addToListRender(addToList) +:
              begin_section(repeater.expandedShortName) +:
              renderHtmls(singleton, sectionNumber)
          case s: Singleton[_]                     => renderHtmls(s, sectionNumber)
          case nextRepeaterAfterRepeater(repeater) => begin_section(repeater.expandedShortName) :: Nil
          case r: Repeater[_]                      => Nil
        }
    }
  }

  def summaryForNotificationPdf(
    validationResult: ValidationResult,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo],
    maybeAccessCode: Option[AccessCode],
    formTemplate: FormTemplate,
    envelope: Envelope,
    obligations: Obligations,
    reviewerComments: Option[String] = None,
    pdfFieldIds: List[FormComponentId]
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[Html] = {

    def renderHtmls(fields: List[FormComponent])(implicit l: LangADT): List[Html] = {
      val rows = fields
        .flatMap(
          formComponent =>
            getSummaryListRows(
              formComponent,
              formTemplate._id,
              formModelVisibilityOptics,
              maybeAccessCode,
              SectionNumber(0),
              SectionTitle4Ga(""),
              obligations,
              validationResult,
              envelope
          ))

      List(new govukSummaryList()(SummaryList(rows)))
    }

    /* val allFormComponents =
     *   formTemplate.expandFormTemplateFull.formComponentsLookupFull
     *
     * val nonEmptyFormComponentIds =
     *   data.data.data.toList
     *     .filter {
     *       _._2.toSeq.map(_.nonEmpty).head
     *     }
     *     .map(_._1)
     *
     * val nonEmptyFormComponents: List[(FormComponentId, FormComponent)] = nonEmptyFormComponentIds.flatMap { fcId =>
     *   allFormComponents.find(_._1 == fcId)
     * }
     *
     * val filteredFormComponents: List[FormComponent] = pdfFieldIds
     *   .flatMap { fcId =>
     *     nonEmptyFormComponents.find(_._1.value.startsWith(fcId.value))
     *   }
     *   .map(_._2) */

    val filteredFormComponents: List[FormComponent] = ???

    renderHtmls(filteredFormComponents)
  }

  private def getSummaryListRows[D <: DataOrigin](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    validationResult: ValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[SummaryListRow] = {

    val formFieldValidationResult: FormFieldValidationResult = validationResult(formComponent)

    formComponent match {
      case IsText(_) =>
        getTextSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope)

      case IsTextArea(_) =>
        getTextAreaSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope)

      case IsUkSortCode(_) =>
        getUkSortCodeSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsDate(_) =>
        getDateSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsAddress(_) =>
        getAddressSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult)

      case IsInformationMessage(_) =>
        List(SummaryListRow())

      case IsFileUpload() =>
        getFileUploadSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          envelope)

      case IsHmrcTaxPeriod(h) =>
        getHmrcTaxPeriodSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          obligations,
          h,
          envelope)

      case IsChoice(choice) =>
        getChoiceSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          choice)

      case IsRevealingChoice(rc) =>
        getRevealingChoiceSummaryListRows(
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          formFieldValidationResult,
          validationResult,
          rc,
          obligations,
          envelope
        )

      case IsGroup(group) =>
        getGroupSummaryListRows(
          group,
          formComponent,
          formTemplateId,
          formModelVisibilityOptics,
          maybeAccessCode,
          sectionNumber,
          sectionTitle4Ga,
          obligations,
          formFieldValidationResult,
          validationResult,
          envelope
        )

    }
  }

  private def checkErrors(fieldValue: FormComponent, formFieldValidationResult: FormFieldValidationResult) =
    formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline(s"${fieldValue.id.value}-error-message", e, Seq("error-message"))
    }

  private def getLabel(fieldValue: FormComponent)(implicit lise: SmartStringEvaluator) =
    fieldValue.shortName.map(ls => ls.value).getOrElse(fieldValue.label.value)

  private def getVisuallyHiddenText(fieldValue: FormComponent)(implicit lise: SmartStringEvaluator) =
    Some(fieldValue.shortName.map(ls => ls.value).getOrElse(fieldValue.label.value))

  private def getKeyClasses(hasErrors: Boolean) =
    if (hasErrors)
      "summary--error"
    else
      ""

  private def getTextSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = getLabel(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.mkString(" ") else formatText(formFieldValidationResult, envelope)

    List(
      summaryListRow(
        label,
        value,
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))

  }

  private def getTextAreaSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = getLabel(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val currentValueLines = formatText(formFieldValidationResult, envelope).split("\\R")

    val currentValue = if (currentValueLines.nonEmpty) {
      currentValueLines.init.map { line =>
        s"$line<br>"
      }.mkString + currentValueLines.last
    } else ""

    val value = if (hasErrors) errors.mkString(" ") else currentValue

    List(
      summaryListRow(
        label,
        value,
        visuallyHiddenText,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getUkSortCodeSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = getLabel(fieldValue)

    val visuallyHiddenText = getVisuallyHiddenText(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    val currentValue = UkSortCode
      .fields(fieldValue.modelComponentId.indexedComponentId) // TODO JoVl, this is weird, let's use MultiValueId instead
      .toList
      .map { fieldId =>
        formFieldValidationResult.getCurrentValue(MultiFieldId.whatIsThis(fieldId))
      }
      .mkString("-")

    val value = if (hasErrors) errors.mkString(" ") else currentValue

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getDateSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(fieldValue, formFieldValidationResult)

    val label = getLabel(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)

    def safeId(atom: Atom) = MultiFieldId.whatIsThis(fieldValue.atomicFormComponentId(atom))

    def monthKey = getMonthValue(formFieldValidationResult.getCurrentValue(safeId(Date.month)))

    val value =
      if (hasErrors)
        errors.head.toString
      else {
        val day = renderMonth(formFieldValidationResult.getCurrentValue(safeId(Date.day)))
        val month = messages(s"date.$monthKey")
        val year = formFieldValidationResult.getCurrentValue(safeId(Date.year))

        s"$day $month $year"
      }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getAddressSummaryListRows[D <: DataOrigin](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = checkErrors(formComponent, formFieldValidationResult)

    val label = formComponent.shortName.map(ls => ls.value.capitalize).getOrElse(formComponent.label.value)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) {
      errors.mkString(" ")
    } else {
      Address
        .renderToString(formComponent, formFieldValidationResult)
        .mkString("", "<br>", "<br>")
    }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getFileUploadSummaryListRows[D <: DataOrigin](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq("error-message"))
    }

    val label = getLabel(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value = if (hasErrors) errors.mkString(" ") else envelope.userFileName(formComponent)

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getHmrcTaxPeriodSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    obligations: Obligations,
    h: HmrcTaxPeriod,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = getLabel(fieldValue)

    val keyClasses = getKeyClasses(hasErrors)
    val periodId = TaxPeriodHelper.formatTaxPeriodOutput(formFieldValidationResult, envelope)

    val maybeObligation = obligations.findByPeriodKey(h, periodId)

    val value =
      if (hasErrors)
        errors.mkString(" ")
      else
        maybeObligation.fold("Value Lost!") { od =>
          messages("generic.From") + " " + formatDate(od.inboundCorrespondenceFromDate) + " " +
            messages("generic.to") + " " + formatDate(od.inboundCorrespondenceToDate)
        }

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (fieldValue.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getChoiceSummaryListRows[D <: DataOrigin](
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    choice: Choice
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
      errorInline("summary", e, Seq())
    }

    val label = getLabel(formComponent)

    val keyClasses = getKeyClasses(hasErrors)

    val value =
      if (hasErrors)
        errors.mkString(" ")
      else
        choice.renderToString(formComponent, formFieldValidationResult).map(s => s"<p>$s</p>").mkString

    List(
      summaryListRow(
        label,
        value,
        None,
        keyClasses,
        "",
        "",
        if (formComponent.onlyShowOnSummary)
          Nil
        else
          List(
            (
              uk.gov.hmrc.gform.gform.routes.FormController
                .form(
                  formTemplateId,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  SuppressErrors.Yes,
                  FastForward.Yes),
              if (formComponent.editable) messages("summary.change") else messages("summary.view")))
      ))
  }

  private def getRevealingChoiceSummaryListRows[D <: DataOrigin](
    fieldValue: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    formFieldValidationResult: FormFieldValidationResult,
    validationResult: ValidationResult,
    rc: RevealingChoice,
    obligations: Obligations,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator): List[SummaryListRow] = {

    val indices = formFieldValidationResult.getComponentFieldIndices(fieldValue.id)

    val selections: List[Option[List[SummaryListRow]]] = rc.options
      .zip(indices)
      .map {
        case (element, index) =>
          val hasErrors = formFieldValidationResult.isNotOk

          val errors = formFieldValidationResult.fieldErrors.toList.map { e =>
            errorInline("summary", e, Seq())
          }

          val label = getLabel(fieldValue)

          val keyClasses = getKeyClasses(hasErrors)

          val value =
            if (hasErrors)
              errors.mkString(" ")
            else
              element.choice.value

          formFieldValidationResult
            .getOptionalCurrentValue(MultiFieldId.indexed(fieldValue.id, index))
            .map { _ =>
              val revealingFields = element.revealingFields.filterNot(_.hideOnSummary).flatMap {
                getSummaryListRows(
                  _,
                  formTemplateId,
                  formModelVisibilityOptics,
                  maybeAccessCode,
                  sectionNumber,
                  sectionTitle4Ga,
                  obligations,
                  validationResult,
                  envelope
                )
              }

              summaryListRow(
                label,
                value,
                None,
                keyClasses,
                "",
                "",
                if (fieldValue.onlyShowOnSummary)
                  Nil
                else
                  List(
                    (
                      uk.gov.hmrc.gform.gform.routes.FormController
                        .form(
                          formTemplateId,
                          maybeAccessCode,
                          sectionNumber,
                          sectionTitle4Ga,
                          SuppressErrors.Yes,
                          FastForward.Yes),
                      if (fieldValue.editable) messages("summary.change") else messages("summary.view")))
              ) +: revealingFields
            }
      }

    selections.collect { case Some(v) => v }.flatten
  }

  private def getGroupSummaryListRows[D <: DataOrigin](
    group: Group,
    formComponent: FormComponent,
    formTemplateId: FormTemplateId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    obligations: Obligations,
    formFieldValidationResult: FormFieldValidationResult,
    validationResult: ValidationResult,
    envelope: Envelope
  )(
    implicit
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): List[SummaryListRow] = {

    val hasErrors = formFieldValidationResult.isNotOk

    val keyClasses = getKeyClasses(hasErrors)

    val label = group.repeatLabel.map(_.value).getOrElse(getLabel(formComponent))

    val visuallyHiddenText = Some(label)

    formComponent.presentationHint match {
      case Some(hints) if hints.contains(SummariseGroupAsGrid) =>
        val formFieldValidationResults: List[FormFieldValidationResult] = group.fields.map(validationResult.apply)

        val errorResults = formFieldValidationResults.filter(_.isNotOk)

        val value =
          errorResults.headOption match {
            case None =>
              formFieldValidationResults.map(ffvr => s"${TextFormatter.formatText(ffvr, envelope)}<br>").mkString
            case Some(formFieldValidationResult) =>
              val errors = checkErrors(formComponent, formFieldValidationResult)
              errors.mkString(" ")
          }

        if (value.nonEmpty) {
          List(
            summaryListRow(
              label,
              value,
              visuallyHiddenText,
              keyClasses,
              "",
              "",
              if (formComponent.onlyShowOnSummary)
                Nil
              else
                List(
                  (
                    uk.gov.hmrc.gform.gform.routes.FormController
                      .form(
                        formTemplateId,
                        maybeAccessCode,
                        sectionNumber,
                        sectionTitle4Ga,
                        SuppressErrors.Yes,
                        FastForward.Yes),
                    if (formComponent.editable) messages("summary.change") else messages("summary.view")))
            )
          )

        } else List(SummaryListRow())

      case _ =>
        val rows = group.fields.flatMap { formComponent =>
          getSummaryListRows(
            formComponent,
            formTemplateId,
            formModelVisibilityOptics,
            maybeAccessCode,
            sectionNumber,
            sectionTitle4Ga,
            obligations,
            validationResult,
            envelope
          )
        }

        val label = getLabel(formComponent)
        if (label.nonEmpty && formComponent.modelComponentId.maybeIndex.fold(false)(_ === 1)) {
          val customKeyClasses = "summary-group-label"

          summaryListRow(label, "", None, customKeyClasses, "", "", Nil) :: rows
        } else rows
    }
  }

}

class NextRepeaterAfterRepeater(formModel: FormModel[Visibility]) {
  def unapply(page: PageModel[Visibility]): Option[Repeater[Visibility]] =
    page match {
      case r: Repeater[_] => formModel.repeaterFor(r.index + 1, r.source.id)
      case _              => None

    }
}

class IsFirstSingletonOfAddToList(sectionNumber: SectionNumber, formModel: FormModel[Visibility]) {
  val lookup: Map[AddToListId, Int] = formModel.firstsAddToList
  def unapply(page: PageModel[Visibility]): Option[(Section.AddToList, Singleton[Visibility], Repeater[Visibility])] =
    page match {
      case s: Singleton[_] =>
        for {
          addToList <- s.sourceIsAddToList
          a         <- lookup.get(addToList.id)
          repeater  <- formModel.repeaterFor(1, addToList.id)
          res       <- if (a === sectionNumber.value) Some((addToList, s, repeater)) else None
        } yield res
      case _ => None

    }
}
