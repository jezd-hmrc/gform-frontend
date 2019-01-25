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

package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.frontend.controller.FrontendController

class FileUploadController(
  fileUploadService: FileUploadService,
  auth: AuthenticatedRequestActions,
  gformConnector: GformConnector
) extends FrontendController {

  def deleteFile(
    formTemplateId: FormTemplateId,
    lang: Option[String],
    maybeAccessCode: Option[AccessCode],
    fileId: FileId
  ) = auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
    for {
      form <- gformConnector.getForm(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
      _    <- fileUploadService.deleteFile(form.envelopeId, fileId)
    } yield NoContent
  }

}
