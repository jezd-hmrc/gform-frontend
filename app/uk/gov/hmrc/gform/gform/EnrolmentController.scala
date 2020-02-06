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

package uk.gov.hmrc.gform.gform

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ EitherT, Kleisli, NonEmptyList, ReaderT }
import cats.instances.future._
import cats.instances.list._
import cats.mtl.implicits._
import cats.mtl.{ ApplicativeAsk, FunctorRaise }
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.validated._
import cats.{ Applicative, Monad, Traverse }
import play.api.i18n.I18nSupport
import play.api.mvc.{ AnyContent, MessagesControllerComponents, Request }
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gform.processor.EnrolmentResultProcessor
import uk.gov.hmrc.gform.graph.{ Recalculation }
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, PageModel, Visibility }
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormModelOptics, ThirdPartyData, ValidatorsResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluatorFactory
import uk.gov.hmrc.gform.sharedmodel.taxenrolments.TaxEnrolmentsResponse
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, GetEmailCodeFieldMatcher, ValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

sealed trait SubmitEnrolmentError
private case object NoIdentifierProvided extends SubmitEnrolmentError
private case class RegimeIdNotMatch(identifier: IdentifierRecipe) extends SubmitEnrolmentError
private case class EnrolmentFormNotValid(errors: GformError) extends SubmitEnrolmentError

case class Env(
  formTemplate: FormTemplate,
  retrievals: MaterialisedRetrievals,
  formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo])

class EnrolmentController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  enrolmentService: EnrolmentService,
  appConfig: AppConfig,
  recalculation: Recalculation[Future, Throwable],
  taxEnrolmentConnector: TaxEnrolmentsConnector,
  ggConnector: GovernmentGatewayConnector,
  frontendAppConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents,
  smartStringEvaluatorFactory: SmartStringEvaluatorFactory
)(
  implicit ec: ExecutionContext
) extends FrontendController(messagesControllerComponents) {

  type Ctx[A] = ReaderT[Future, Env, A]
  type EnrolM[A] = EitherT[Ctx, SubmitEnrolmentError, A]

  private def liftEM[A](a: Future[A]): EnrolM[A] = EitherT.liftF(Kleisli(Function.const(a)))

  private def enrolmentConnect(implicit hc: HeaderCarrier): EnrolmentConnect[EnrolM] =
    new EnrolmentConnect[EnrolM] {
      def enrolGGUser(
        request: TaxEnrolment,
        service: ServiceId,
        retrievals: MaterialisedRetrievals): EnrolM[ServiceCallResponse[TaxEnrolmentsResponse]] =
        liftEM(taxEnrolmentConnector.enrolGGUser(request, service, retrievals))
    }

  private def ggConnect(implicit hc: HeaderCarrier): GGConnect[EnrolM] =
    new GGConnect[EnrolM] {
      def enrolGGUser(request: GGEnrolmentRequest): EnrolM[HttpResponse] =
        liftEM(ggConnector.enrolGGUser(request))
    }

  import i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId) =
    auth.asyncGGAuth(formTemplateId) { implicit request: Request[AnyContent] => implicit l => cache =>
      cache.formTemplate.authConfig match {
        case HasEnrolmentSection((_, enrolmentSection, _, _)) =>
          Ok(
            renderEnrolmentSection(
              cache.formTemplate,
              cache.retrievals,
              enrolmentSection,
              FormModelOptics.empty,
              Nil,
              Nil,
              ValidatorsResult.empty.valid)
          ).pure[Future]
        case _ =>
          Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
            .flashing("formTitle" -> cache.formTemplate.formName.value)
            .pure[Future]
      }
    }

  private def renderEnrolmentSection(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    enrolmentSection: EnrolmentSection,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    errors: List[(FormComponent, FormFieldValidationResult)],
    globalErrors: List[ErrorLink],
    validatedType: ValidatedType[ValidatorsResult])(implicit request: Request[_], l: LangADT) = {
    implicit val sse = smartStringEvaluatorFactory(
      formModelOptics.formModelVisibilityOptics,
      retrievals,
      ThirdPartyData.empty,
      None,
      EnvelopeId("empty"),
      formTemplate)

    renderer
      .renderEnrolmentSection(
        formTemplate,
        retrievals,
        enrolmentSection,
        formModelOptics,
        errors,
        globalErrors,
        ValidationResult.empty
      )
  }

  def submitEnrolment(formTemplateId: FormTemplateId) =
    auth.asyncGGAuth(formTemplateId) { implicit request => implicit l => cache =>
      import cache._
      val checkEnrolment: ServiceId => NonEmptyList[Identifier] => EnrolM[CheckEnrolmentsResult] =
        serviceId => identifiers => EitherT.liftF(Kleisli(_ => auth.checkEnrolment(serviceId, identifiers)))

      val genesisFormModel
        : FormModel[DataExpanded] = ??? // TODO JoVl how to get form model here, perhaps implement other versioon of processResponseDataFromBody???

      processResponseDataFromBody(request, genesisFormModel) { requestRelatedData => variadicFormData =>
        formTemplate.authConfig match {
          case HasEnrolmentSection((serviceId, enrolmentSection, postCheck, lfcev)) =>
            def handleContinueWithData(formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo]) = {
              implicit val sse =
                smartStringEvaluatorFactory(
                  formModelVisibilityOptics,
                  cache.retrievals,
                  ThirdPartyData.empty,
                  None,
                  EnvelopeId(""),
                  formTemplate)

              implicit val EC = enrolmentConnect
              implicit val GGC = ggConnect

              val allFields = Fields.flattenGroups(enrolmentSection.fields)
              val pageModel: PageModel[Visibility] = ???
              val validationResultF: Future[ValidatedType[Unit]] = validationService.validatePageModelComponents(
                pageModel,
                formModelVisibilityOptics,
                cache.toCacheData,
                Envelope.empty,
                GetEmailCodeFieldMatcher.noop
              )(hc, request2Messages, l, sse)
              val enrolmentResultProcessor = new EnrolmentResultProcessor(
                renderEnrolmentSection,
                formTemplate,
                retrievals,
                enrolmentSection,
                formModelVisibilityOptics,
                frontendAppConfig)
              for {
                validationResult <- validationResultF
                res <- processValidation(
                        serviceId,
                        enrolmentSection,
                        postCheck,
                        checkEnrolment(serviceId),
                        validationResult,
                        lfcev,
                        retrievals)
                        .fold(
                          enrolmentResultProcessor.recoverEnrolmentError,
                          enrolmentResultProcessor.processEnrolmentResult
                        )
                        .run(Env(formTemplate, retrievals, formModelVisibilityOptics))
              } yield res
            }

            /* def handleContinue =
             *   for {
             *     data <- recalculation
             *              .recalculateFormDataWithLookup(
             *                dataRaw,
             *                formTemplate,
             *                retrievals,
             *                ThirdPartyData.empty,
             *                EnvelopeId(""),
             *                enrolmentSection.fields
             *                  .map(fc => fc.id -> fc)
             *                  .toMap
             *              )
             *     res <- handleContinueWithData(data)
             *   } yield res */
            requestRelatedData.get("save") match {
              case "Continue" => ??? // handleContinue
              case _          => Future.successful(BadRequest("Cannot determine action"))
            }
          case _ =>
            Future.successful(
              Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
                .flashing("formTitle" -> formTemplate.formName.value)
            )
        }
      }
    }

  private def validateIdentifiers[F[_]: Applicative](
    identifiers: NonEmptyList[(IdentifierRecipe, Identifier)],
    postCheck: EnrolmentPostCheck
  )(implicit FR: FunctorRaise[F, SubmitEnrolmentError]): F[Unit] =
    postCheck match {
      case NoCheck => ().pure[F]
      case RegimeIdCheck(regimeId) =>
        val (identifierRecipe, identifier) = identifiers.head
        if (identifier.value.drop(2).startsWith(regimeId.value))
          ().pure[F]
        else
          FR.raise(RegimeIdNotMatch(identifierRecipe))
    }

  private def processValidation[F[_]: Monad: EnrolmentConnect: GGConnect](
    serviceId: ServiceId,
    enrolmentSection: EnrolmentSection,
    postCheck: EnrolmentPostCheck,
    checkEnrolment: NonEmptyList[Identifier] => F[CheckEnrolmentsResult],
    validationResult: ValidatedType[Unit],
    enrolmentAction: EnrolmentAction,
    retrievals: MaterialisedRetrievals
  )(
    implicit hc: HeaderCarrier,
    request: Request[AnyContent],
    AA: ApplicativeAsk[F, Env],
    FR: FunctorRaise[F, SubmitEnrolmentError]): F[CheckEnrolmentsResult] = {

    def tryEnrolment(verifiers: List[Verifier], identifiers: NonEmptyList[Identifier]): F[CheckEnrolmentsResult] = ???
    /* for {
     *     enrolmentResponse <- enrolmentService.enrolUser[F](serviceId, identifiers, verifiers, retrievals)
     *     result <- enrolmentResponse match {
     *                case ServiceResponse(TaxEnrolmentsResponse.Success)  => checkEnrolment(identifiers)
     *                case ServiceResponse(TaxEnrolmentsResponse.Conflict) => CheckEnrolmentsResult.Conflict.pure[F]
     *                case ServiceResponse(TaxEnrolmentsResponse.InvalidIdentifiers) =>
     *                  CheckEnrolmentsResult.InvalidIdentifiers.pure[F]
     *                case ServiceResponse(TaxEnrolmentsResponse.InvalidCredentials) =>
     *                  CheckEnrolmentsResult.InvalidCredentials.pure[F]
     *                case _ =>
     *                  CheckEnrolmentsResult.Failed.pure[F]
     *              }
     *   } yield result
     *
     * validationResult match {
     *   case Invalid(errors) => FR.raise(EnrolmentFormNotValid(errors))
     *   case Valid(()) =>
     *     for {
     *       idenVer <- extractIdentifiersAndVerifiers(enrolmentSection)
     *       (identifierss, verifiers) = idenVer
     *       identifiers = identifierss.map(_._2)
     *       _             <- validateIdentifiers[F](identifierss, postCheck)
     *       initialResult <- tryEnrolment(verifiers, identifiers)
     *       reattemptResult <- (initialResult, enrolmentAction) match {
     *                           case (CheckEnrolmentsResult.InvalidIdentifiers, LegacyFcEnrolmentVerifier(value)) =>
     *                             tryEnrolment(List(Verifier(value, "FC")), identifiers)
     *                           case _ =>
     *                             initialResult.pure[F]
     *                         }
     *     } yield reattemptResult
     *
     * } */
    ???
  }

  private def purgeEmpty[F[_]: Applicative](
    xs: NonEmptyList[(IdentifierRecipe, Identifier)]
  )(
    implicit FR: FunctorRaise[F, SubmitEnrolmentError]
  ): F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
    xs.toList.filterNot(_._2.value.isEmpty) match {
      case Nil    => FR.raise(NoIdentifierProvided)
      case h :: t => NonEmptyList(h, t).pure[F]
    }

  /* private def extractIdentifiersAndVerifiers[F[_]: Monad](
 *   enrolmentSection: EnrolmentSection
 * )(
 *   implicit hc: HeaderCarrier,
 *   evaluator: Evaluator,
 *   AA: ApplicativeAsk[F, Env],
 *   FR: FunctorRaise[F, SubmitEnrolmentError]): F[(NonEmptyList[(IdentifierRecipe, Identifier)], List[Verifier])] = {
 *   def evaluate[A, B, G[_]: Traverse](xs: G[A])(g: A => Expr, f: A => String => B): F[G[B]] =
 *     for {
 *       env <- AA.ask
 *       res <- xs.traverse { x =>
 *               val fcId = FormComponentId("dummy")
 *               val convertible: Convertible =
 *                 evaluator.eval(
 *                   //Set.empty,
 *                   env.data.formModelVisibilityOptics,
 *                   fcId,
 *                   //env.data.data,
 *                   env.retrievals,
 *                   env.formTemplate,
 *                   //env.data.formModel,
 *                   ThirdPartyData.empty,
 *                   EnvelopeId("")
 *                 )(g(x))
 *               Convertible
 *                 .asString(convertible, env.data.formModelVisibilityOptics)
 *                 .map {
 *                   case Some(NewValue(value)) => f(x)(value)
 *                   case _                     => f(x)("")
 *                 }
 *             }
 *     } yield res
 *
 *   val allIdentifiers: F[NonEmptyList[(IdentifierRecipe, Identifier)]] =
 *     evaluate(enrolmentSection.identifiers)(
 *       _.value,
 *       identifier => value => (identifier, Identifier(identifier.key, value)))
 *
 *   val allVerifiers: F[List[Verifier]] =
 *     evaluate(enrolmentSection.verifiers)(
 *       _.value,
 *       verifier =>
 *         value =>
 *           if (value.nonEmpty)
 *             List(Verifier(verifier.key, value))
 *           else Nil).map(_.flatten)
 *
 *   for {
 *     identifiers       <- allIdentifiers
 *     verifiers         <- allVerifiers
 *     purgedIdentifiers <- purgeEmpty[F](identifiers)
 *   } yield (purgedIdentifiers, verifiers)
 * } */
}
