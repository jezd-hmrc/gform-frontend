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

package pact.uk.gov.hmrc.gform

import com.itv.scalapact.ScalaPactForger._
import org.scalatest.concurrent.ScalaFutures
import play.api.libs.json.Json
import uk.gov.hmrc.gform.SpecWithFakeApp
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionData, Variables }
import uk.gov.hmrc.gform.wshttp.WSHttp

class GFormConnectorPactTest extends SpecWithFakeApp with ScalaFutures {

  it should "should be able to submit data to gform backend" in {
    lazy val htmlForm = "<hmtl>something</html>"
    val submissionData =
      SubmissionData(htmlForm, Variables(Json.parse("""{"user":{"enrolledIdentifier":"ITC"}}""")))

    forgePact
      .between("gform-frontend")
      .and("gform")
      .addInteraction(
        interaction
          .description("Submitting SubmissionData")
          .uponReceiving(
            POST,
            "/gform/forms/123/submission-pdf",
            None,
            Map("customerId" -> "cid", "Content-Type" -> "application/json"),
            Some(Json.toJson(submissionData).toString),
            None
          )
          .given("Form 123 exists")
          .willRespondWith(204)
      )
      .runConsumerTest { mockConfig =>
        val connector = new GformConnector(WSHttp, s"${mockConfig.baseUrl}/gform")
        val eventualResponse = connector.submitFormWithPdf(FormId("123"), CustomerId("cid"), submissionData, None)

        whenReady(eventualResponse) { r =>
          r.get.status should be(204)
          r.get.body should be("")
        }
      }
  }
}