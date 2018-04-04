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

package uk.gov.hmrc.vatapi.models

import org.joda.time.LocalDate
import org.joda.time.format.ISODateTimeFormat
import play.api.Logger
import uk.gov.hmrc.vatapi.config.AppContext

import scala.util.Try

case class FinancialDataQueryParams(from: LocalDate, to: LocalDate) {
  val map: Map[SourceId, LocalDate] = Map("from" -> from, "to" -> to)
}

object FinancialDataQueryParams {
  val dateRegex: SourceId = """^\d{4}-\d{2}-\d{2}$"""
  val minDate: LocalDate = LocalDate.parse(AppContext.mtdDate, ISODateTimeFormat.date())

  def from(fromOpt: OptEither[String], toOpt: OptEither[String]): Either[String, FinancialDataQueryParams] = {

    Logger.debug(s"[FinancialDataQueryParams][from] FromDate:$fromOpt  toDate:$toOpt  minDate: $minDate")

    val from = checkMinFromDate(dateQueryParam(fromOpt, "DATE_FROM_INVALID"))
    val to = checkFutureToDate(dateQueryParam(toOpt, "DATE_TO_INVALID"))

    val errors = for {
      paramOpt <- Seq(from, to, validDateRange(from, to))
      param <- paramOpt
      if param.isLeft
    } yield param.left.get

    if (errors.isEmpty) {
      Right(FinancialDataQueryParams(from.map(_.right.get).get, to.map(_.right.get).get))
    } else {
      Left(errors.head)
    }
  }

  private def dateQueryParam(dateOpt: OptEither[String], errorCode: String): OptEither[LocalDate] = {
    val paramValue = dateOpt match {
      case Some(value) =>
        val dateString = value.right.get
        if (dateString.matches(dateRegex))
          Try(Right(LocalDate.parse(dateString))).getOrElse(Left(errorCode))
        else
          Left(errorCode)
      case None => Left(errorCode)
    }
    Some(paramValue)
  }

  def validDateRange(fromOpt: OptEither[LocalDate], toOpt: OptEither[LocalDate]): Option[Either[SourceId, Unit] with Product with Serializable] = {


    for {
      fromVal <- fromOpt
      if fromVal.isRight
      toVal <- toOpt
      if toVal.isRight
    } yield
      (fromVal.right.get, toVal.right.get) match {
        case (from, to) if !from.isBefore(to) || from.plusYears(1).minusDays(1).isBefore(to) =>
          Left("DATE_RANGE_INVALID")
        case _ => Right(()) // object wrapped in Right irrelevant
      }
  }

  def checkMinFromDate(date: OptEither[LocalDate]): OptEither[LocalDate] = {
    val out = date match {
      case Some(value) =>
        value match {
          case Right(d) if d.isBefore(minDate) => Left("DATE_FROM_INVALID")
          case _ => value
        }
      case None => Left("DATE_FROM_INVALID")
    }
    Some(out)
  }

  def checkFutureToDate(date: OptEither[LocalDate]): OptEither[LocalDate] = {
    val out = date match {
      case Some(value) =>
        value match {
          case Right(d) if d.isAfter(LocalDate.now()) => Left("DATE_TO_INVALID")
          case _ => value
        }
      case None => Left("DATE_TO_INVALID")
    }
    Some(out)
  }

}