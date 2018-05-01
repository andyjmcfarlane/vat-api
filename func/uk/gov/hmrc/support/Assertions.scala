package uk.gov.hmrc.support

import com.github.tomakehurst.wiremock.client.WireMock.verify
import com.github.tomakehurst.wiremock.matching.RequestPatternBuilder
import org.json.{JSONArray, JSONObject}
import org.scalatest.{Assertion, Matchers}
import org.skyscreamer.jsonassert.JSONAssert.assertEquals
import org.skyscreamer.jsonassert.JSONCompareMode.LENIENT
import play.api.libs.json._
import uk.gov.hmrc.api.controllers.ErrorNotFound
import uk.gov.hmrc.http.HttpResponse
import uk.gov.hmrc.vatapi.models.ErrorNotImplemented

import scala.collection.mutable
import scala.util.matching.Regex

class Assertions(request: String, response: HttpResponse) (
    implicit urlPathVariables: mutable.Map[String, String])
    extends UrlInterpolation with Matchers {
    def jsonBodyIsEmptyObject(): Assertion = response.json shouldBe Json.obj()

    def jsonBodyIsEmptyArray(): Assertion = response.json shouldBe JsArray()

    def responseContainsHeader(name: String, pattern: Regex): Assertions = {
      response.header(name) match {
        case Some(h) => h should fullyMatch regex pattern
        case _ => fail(s"Header [$name] not found in the response headers")
      }
      this
    }

    def butResponseHasNo(sourceName: String, summaryName: String = ""): Assertions = {
      val jsvOpt =
      // FIXME: use \\
        if (summaryName.isEmpty)
          (response.json \ "_embedded" \ sourceName).toOption
        else (response.json \ "_embedded" \ sourceName \ summaryName).toOption

      jsvOpt match {
        case Some(v) =>
          v.asOpt[List[String]] match {
            case Some(list) => list.isEmpty shouldBe true
            case _ =>
          }
        case None => ()
      }
      this
    }

    def bodyIsError(code: String): Assertions = body(_ \ "code").is(code)

    def isValidationError(error: (String, String)): Assertions =
      isValidationError(error._1, error._2)

    def isValidationError(path: String, code: String): Assertions = {
      statusIs(400).contentTypeIsJson().body(_ \ "code").is("INVALID_REQUEST")

      val errors = (response.json \ "errors").toOption
      errors match {
        case None => fail("didn't find 'errors' element in the json response")
        case Some(e) =>
          (e(0) \ "path").toOption shouldBe Some(JsString(path))
          (e(0) \ "code").toOption shouldBe Some(JsString(code))
      }
      this
    }

    def isBadRequest(path: String, code: String): Assertions = {
      statusIs(400)
        .contentTypeIsJson()
        .body(_ \ "path")
        .is(path)
        .body(_ \ "code")
        .is(code)
      this
    }

    def isBadRequest(code: String): Assertions = {
      statusIs(400).contentTypeIsJson().body(_ \ "code").is(code)
      this
    }

    def isBadRequest: Assertions = {
      isBadRequest("INVALID_REQUEST")
    }

    def isNotFound: Assertions = {
      statusIs(404).contentTypeIsJson().bodyIsError(ErrorNotFound.errorCode)
      this
    }

    def isNotImplemented: Assertions = {
      statusIs(501)
        .contentTypeIsJson()
        .bodyIsError(ErrorNotImplemented.errorCode)
      this
    }

    def contentTypeIsXml(): Assertions = contentTypeIs("application/xml")

    def contentTypeIsJson(): Assertions = contentTypeIs("application/json")

    def contentTypeIsHalJson(): Assertions = contentTypeIs("application/hal+json")

    def noInteractionsWithExternalSystems(): Unit = {
      verify(0, RequestPatternBuilder.allRequests())
    }

    def bodyIs(expectedBody: String): Assertions = {
      response.body shouldBe expectedBody
      this
    }

    def bodyIs(expectedBody: JsValue): Assertions = {
      (response.json match {
        case JsObject(fields) => response.json.as[JsObject] - "_links" - "id"
        case json => json
      }) shouldEqual expectedBody
      this
    }

    def bodyIsLike(expectedBody: String): Assertions = {
      response.json match {
        case JsArray(_) =>
          assertEquals(expectedBody, new JSONArray(response.body), LENIENT)
        case _ =>
          assertEquals(expectedBody, new JSONObject(response.body), LENIENT)
      }
      this
    }

    def bodyHasLink(rel: String, href: String): Assertions = {
      getLinkFromBody(rel) shouldEqual Some(interpolated(href))
      this
    }

    def bodyHasPath[T](path: String, value: T)(
      implicit reads: Reads[T]): Assertions = {
      extractPathElement(path) shouldEqual Some(value)
      this
    }

    def bodyHasPath(path: String, valuePattern: Regex): Assertions = {
      extractPathElement[String](path) match {
        case Some(x) =>
          valuePattern findFirstIn x match {
            case Some(v) =>
            case None => fail(s"$x did not match pattern")
          }
        case None => fail(s"No value found for $path")
      }
      this
    }

    def bodyDoesNotHavePath[T](path: String)(implicit reads: Reads[T]): Assertions = {
      extractPathElement[T](path) match {
        case Some(x) => fail(s"$x match found")
        case None =>
      }
      this
    }

    private def extractPathElement[T](path: String)(
      implicit reads: Reads[T]): Option[T] = {
      val pathSeq =
        path.filter(!_.isWhitespace).split('\\').toSeq.filter(!_.isEmpty)

      def op(js: Option[JsValue], pathElement: String): Option[JsValue] = {
        val pattern = """(.*)\((\d+)\)""".r
        js match {
          case Some(v) =>
            pathElement match {
              case pattern(arrayName, index) =>
                js match {
                  case Some(v) =>
                    if (arrayName.isEmpty) Some(v(index.toInt))
                    else Some((v \ arrayName) (index.toInt))
                  case None => None
                }
              case _ => (v \ pathElement).toOption
            }
          case None => None
        }
      }

      pathSeq
        .foldLeft(Some(response.json): Option[JsValue])(op)
        .flatMap(jsValue => jsValue.asOpt[T])
    }

    private def getLinkFromBody(rel: String): Option[String] =
      if (response.body.isEmpty) None
      else
        (for {
          links <- (response.json \ "_links").toOption
          link <- (links \ rel).toOption
          href <- (link \ "href").toOption

        } yield href.asOpt[String]).flatten

    def bodyHasLink(rel: String, hrefPattern: Regex): Assertions = {
      getLinkFromBody(rel) match {
        case Some(href) =>
          interpolated(hrefPattern).r findFirstIn href match {
            case Some(v) =>
            case None => fail(s"$href did not match pattern")
          }
        case None => fail(s"No href found for $rel")
      }
      this
    }

    def bodyHasString(content: String): Assertions = {
      response.body.contains(content) shouldBe true
      this
    }

    def bodyDoesNotHaveString(content: String): Assertions = {
      response.body.contains(content) shouldBe false
      this
    }

    def statusIs(statusCode: Regex): Assertions = {
      withClue(
        s"expected $request to return $statusCode; but got ${response.body}\n") {
        response.status.toString should fullyMatch regex statusCode
      }
      this
    }

    def statusIs(statusCode: Int): Assertions = {
      withClue(
        s"expected $request to return $statusCode; but got ${response.body}\n") {
        response.status shouldBe statusCode
      }
      this
    }

    private def contentTypeIs(contentType: String) = {
      response.header("Content-Type") shouldEqual Some(contentType)
      this
    }

    def body(myQuery: JsValue => JsLookupResult): BodyAssertions = {
      new BodyAssertions(myQuery(response.json).toOption, this)
    }

    def selectFields(myQuery: JsValue => Seq[JsValue]): BodyListAssertions = {
      new BodyListAssertions(myQuery(response.json), this)
    }

    def hasHeader(headerName: String): Assertions = {
      response.allHeaders(headerName) should not be 'empty
      this
    }

    def hasHeaderValues(h: Map[String, Seq[String]]): Assertions = {
      val result = h.map(l => response.allHeaders.exists(_ == l))
      result shouldBe true
      this
    }

    class BodyAssertions(content: Option[JsValue], assertions: Assertions) {
      def is(value: String): Assertions = {
        content match {
          case Some(v) =>
            v.asOpt[String] match {
              case Some(actualValue) => actualValue shouldBe value
              case _ => "" shouldBe value
            }
          case None => ()
        }
        assertions
      }

      def isAbsent(): Assertions = {
        content shouldBe None
        assertions
      }

      def is(value: BigDecimal): Assertions = {
        content match {
          case Some(v) => v.as[BigDecimal] shouldBe value
          case None => fail()
        }
        assertions
      }
    }

    class BodyListAssertions(content: Seq[JsValue], assertions: Assertions) {
      def isLength(n: Int): BodyListAssertions = {
        content.size shouldBe n
        this
      }

      def matches(matcher: Regex): Assertions = {
        content.map(_.as[String]).forall {
          case matcher(_*) => true
          case _ => false
        } shouldBe true

        assertions
      }

      def is(value: String*): Assertions = {
        content.map(con => con.as[String]) should contain theSameElementsAs value
        assertions
      }
    }
}
