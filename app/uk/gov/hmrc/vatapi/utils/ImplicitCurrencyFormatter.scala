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

package uk.gov.hmrc.vatapi.utils

import uk.gov.hmrc.vatapi.models.Amount

trait ImplicitCurrencyFormatter {
  implicit class CurrencyFormatter(x: Amount) {
    def toDesCurrency: String = x.setScale(2).toString()
  }
}

object ImplicitCurrencyFormatter extends ImplicitCurrencyFormatter
