import javax.inject.Inject

import akka.stream.Materializer
import play.api.Logger
import play.api.http.HttpFilters
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class LoggingFilter @Inject() (implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  def apply(nextFilter: (RequestHeader) => Future[Result])
           (requestHeader: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    nextFilter(requestHeader).map { result =>
      val endTime = System.currentTimeMillis
      val requestTime = endTime - startTime
      Logger.info(s"${requestHeader.method} ${requestHeader.uri} " +
        s"took ${requestTime}ms and returned ${result.header.status}")
      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}


import javax.inject.Inject
import play.api.http.DefaultHttpFilters
import play.api.http.EnabledFilters
import play.filters.gzip.GzipFilter

class Filters @Inject() (
                          defaultFilters: EnabledFilters,
                          gzip: GzipFilter,
                          log: LoggingFilter
                        ) extends DefaultHttpFilters(defaultFilters.filters :+ gzip :+ log: _*)