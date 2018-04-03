/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.vatapi.models.des

import uk.gov.hmrc.vatapi.models.Amount
import org.joda.time.DateTime
import play.api.libs.json._
import org.joda.time.DateTimeZone
import uk.gov.hmrc.vatapi.models.dateTimeFormat

case class VatReturnDeclaration(
                                 periodKey: String,
                                 vatDueSales: Amount,
                                 vatDueAcquisitions: Amount,
                                 vatDueTotal: Amount,
                                 vatReclaimedCurrPeriod: Amount,
                                 vatDueNet: Amount,
                                 totalValueSalesExVAT: String,
                                 totalValuePurchasesExVAT: String,
                                 totalValueGoodsSuppliedExVAT: String,
                                 totalAllAcquisitionsExVAT: String,
                                 agentReferenceNumber: Option[String] = None,
                                 receivedAt: DateTime
                               )

object VatReturnDeclaration {

  implicit val format: Format[VatReturnDeclaration] = Json.format[VatReturnDeclaration]

}
