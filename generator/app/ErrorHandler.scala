import javax.inject.Singleton

import io.apibuilder.generator.v0.models.json._
import lib.Validation
import play.api._
import play.api.http.HttpErrorHandler
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.Future

@Singleton
class ErrorHandler extends HttpErrorHandler {

  override def onClientError(request: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    if (statusCode == play.api.http.Status.NOT_FOUND) {
      Future.successful(NotFound)
    } else if (statusCode == play.api.http.Status.BAD_REQUEST) {
      Future.successful(BadRequest(Json.toJson(Validation.serverError("Bad Request"))))
    } else {
      //TODO: Log or not?
      Logger.error(s"FlowClientError: Requesgt ID: ${request.id} Status Code: ${statusCode.toString} Message: $message")
      Future.successful(Status(statusCode)("A client error occurred: " + message))
    }
  }

  override def onServerError(request: RequestHeader, ex: Throwable): Future[Result] = {
    Logger.error(ex.toString, ex)
    Future.successful(InternalServerError(Json.toJson(Validation.serverError())))
  }
}
