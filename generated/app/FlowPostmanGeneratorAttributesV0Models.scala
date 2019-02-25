/**
 * Generated by API Builder - https://www.apibuilder.io
 * Service version: 0.0.1-chuck3
 * apibuilder 0.14.59 app.apibuilder.io/flow/postman-generator-attributes/0.0.1-chuck3/play_2_x_json
 */
package io.flow.postman.generator.attributes.v0.models {

  /**
   * @param username Specifies the username to be used in Postman Collection auth definition. It may
   *        be also a variable reference.
   * @param password Specified the password to be used in Postman Collection auth definition. It may
   *        be also a variable reference. Leave this string empty, when only a username is
   *        needed.
   */
  final case class BasicAuth(
    username: String,
    password: String
  )

  /**
   * @param relatedServiceNamespace ApiBuilder service's namespace, in which a target operation is specified.
   * @param resourceType Resource type, that holds a referenced operation.
   * @param operationMethod Referenced operation method.
   * @param identifierField A field name. When the referenced operation is called, identifier_field is used
   *        to extract the corresponding field from the response JSON.
   */
  final case class ModelReference(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: io.apibuilder.spec.v0.models.Method,
    identifierField: String
  )

  /**
   * @param relatedServiceNamespace ApiBuilder service's namespace, in which a target operation is specified.
   * @param resourceType Resource type, that holds a referenced operation.
   * @param operationMethod Referenced operation method.
   * @param identifierField A field name. When the referenced operation is called, identifier_field is used
   *        to extract the corresponding field from the response JSON.
   * @param path Path from resource
   */
  final case class PathReference(
    relatedServiceNamespace: String,
    resourceType: String,
    operationMethod: io.apibuilder.spec.v0.models.Method,
    identifierField: String,
    path: _root_.scala.Option[String] = None
  )

  sealed trait AttributeName extends _root_.scala.Product with _root_.scala.Serializable

  object AttributeName {

    case object PostmanBasicAuth extends AttributeName { override def toString = "postman-basic-auth" }
    case object ObjectReference extends AttributeName { override def toString = "object-reference" }
    case object OrganizationSetup extends AttributeName { override def toString = "organization-setup" }

    /**
     * UNDEFINED captures values that are sent either in error or
     * that were added by the server after this library was
     * generated. We want to make it easy and obvious for users of
     * this library to handle this case gracefully.
     *
     * We use all CAPS for the variable name to avoid collisions
     * with the camel cased values above.
     */
    final case class UNDEFINED(override val toString: String) extends AttributeName

    /**
     * all returns a list of all the valid, known values. We use
     * lower case to avoid collisions with the camel cased values
     * above.
     */
    val all: scala.List[AttributeName] = scala.List(PostmanBasicAuth, ObjectReference, OrganizationSetup)

    private[this]
    val byName: Map[String, AttributeName] = all.map(x => x.toString.toLowerCase -> x).toMap

    def apply(value: String): AttributeName = fromString(value).getOrElse(UNDEFINED(value))

    def fromString(value: String): _root_.scala.Option[AttributeName] = byName.get(value.toLowerCase)

  }

}

package io.flow.postman.generator.attributes.v0.models {

  package object json {
    import play.api.libs.json.__
    import play.api.libs.json.JsString
    import play.api.libs.json.Writes
    import play.api.libs.functional.syntax._
    import io.apibuilder.spec.v0.models.json._
    import io.flow.postman.generator.attributes.v0.models.json._

    private[v0] implicit val jsonReadsUUID = __.read[String].map(java.util.UUID.fromString)

    private[v0] implicit val jsonWritesUUID = new Writes[java.util.UUID] {
      def writes(x: java.util.UUID) = JsString(x.toString)
    }

    private[v0] implicit val jsonReadsJodaDateTime = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateTimeParser
      dateTimeParser.parseDateTime(str)
    }

    private[v0] implicit val jsonWritesJodaDateTime = new Writes[org.joda.time.DateTime] {
      def writes(x: org.joda.time.DateTime) = {
        import org.joda.time.format.ISODateTimeFormat.dateTime
        val str = dateTime.print(x)
        JsString(str)
      }
    }

    private[v0] implicit val jsonReadsJodaLocalDate = __.read[String].map { str =>
      import org.joda.time.format.ISODateTimeFormat.dateParser
      dateParser.parseLocalDate(str)
    }

    private[v0] implicit val jsonWritesJodaLocalDate = new Writes[org.joda.time.LocalDate] {
      def writes(x: org.joda.time.LocalDate) = {
        import org.joda.time.format.ISODateTimeFormat.date
        val str = date.print(x)
        JsString(str)
      }
    }

    implicit val jsonReadsPostmanGeneratorAttributesAttributeName = new play.api.libs.json.Reads[io.flow.postman.generator.attributes.v0.models.AttributeName] {
      def reads(js: play.api.libs.json.JsValue): play.api.libs.json.JsResult[io.flow.postman.generator.attributes.v0.models.AttributeName] = {
        js match {
          case v: play.api.libs.json.JsString => play.api.libs.json.JsSuccess(io.flow.postman.generator.attributes.v0.models.AttributeName(v.value))
          case _ => {
            (js \ "value").validate[String] match {
              case play.api.libs.json.JsSuccess(v, _) => play.api.libs.json.JsSuccess(io.flow.postman.generator.attributes.v0.models.AttributeName(v))
              case err: play.api.libs.json.JsError => err
            }
          }
        }
      }
    }

    def jsonWritesPostmanGeneratorAttributesAttributeName(obj: io.flow.postman.generator.attributes.v0.models.AttributeName) = {
      play.api.libs.json.JsString(obj.toString)
    }

    def jsObjectAttributeName(obj: io.flow.postman.generator.attributes.v0.models.AttributeName) = {
      play.api.libs.json.Json.obj("value" -> play.api.libs.json.JsString(obj.toString))
    }

    implicit def jsonWritesPostmanGeneratorAttributesAttributeName: play.api.libs.json.Writes[AttributeName] = {
      new play.api.libs.json.Writes[io.flow.postman.generator.attributes.v0.models.AttributeName] {
        def writes(obj: io.flow.postman.generator.attributes.v0.models.AttributeName) = {
          jsonWritesPostmanGeneratorAttributesAttributeName(obj)
        }
      }
    }

    implicit def jsonReadsPostmanGeneratorAttributesBasicAuth: play.api.libs.json.Reads[BasicAuth] = {
      for {
        username <- (__ \ "username").read[String]
        password <- (__ \ "password").read[String]
      } yield BasicAuth(username, password)
    }

    def jsObjectBasicAuth(obj: io.flow.postman.generator.attributes.v0.models.BasicAuth): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "username" -> play.api.libs.json.JsString(obj.username),
        "password" -> play.api.libs.json.JsString(obj.password)
      )
    }

    implicit def jsonWritesPostmanGeneratorAttributesBasicAuth: play.api.libs.json.Writes[BasicAuth] = {
      new play.api.libs.json.Writes[io.flow.postman.generator.attributes.v0.models.BasicAuth] {
        def writes(obj: io.flow.postman.generator.attributes.v0.models.BasicAuth) = {
          jsObjectBasicAuth(obj)
        }
      }
    }

    implicit def jsonReadsPostmanGeneratorAttributesModelReference: play.api.libs.json.Reads[ModelReference] = {
      for {
        relatedServiceNamespace <- (__ \ "related_service_namespace").read[String]
        resourceType <- (__ \ "resource_type").read[String]
        operationMethod <- (__ \ "operation_method").read[io.apibuilder.spec.v0.models.Method]
        identifierField <- (__ \ "identifier_field").read[String]
      } yield ModelReference(relatedServiceNamespace, resourceType, operationMethod, identifierField)
    }

    def jsObjectModelReference(obj: io.flow.postman.generator.attributes.v0.models.ModelReference): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "related_service_namespace" -> play.api.libs.json.JsString(obj.relatedServiceNamespace),
        "resource_type" -> play.api.libs.json.JsString(obj.resourceType),
        "operation_method" -> play.api.libs.json.JsString(obj.operationMethod.toString),
        "identifier_field" -> play.api.libs.json.JsString(obj.identifierField)
      )
    }

    implicit def jsonWritesPostmanGeneratorAttributesModelReference: play.api.libs.json.Writes[ModelReference] = {
      new play.api.libs.json.Writes[io.flow.postman.generator.attributes.v0.models.ModelReference] {
        def writes(obj: io.flow.postman.generator.attributes.v0.models.ModelReference) = {
          jsObjectModelReference(obj)
        }
      }
    }

    implicit def jsonReadsPostmanGeneratorAttributesPathReference: play.api.libs.json.Reads[PathReference] = {
      for {
        relatedServiceNamespace <- (__ \ "related_service_namespace").read[String]
        resourceType <- (__ \ "resource_type").read[String]
        operationMethod <- (__ \ "operation_method").read[io.apibuilder.spec.v0.models.Method]
        identifierField <- (__ \ "identifier_field").read[String]
        path <- (__ \ "path").readNullable[String]
      } yield PathReference(relatedServiceNamespace, resourceType, operationMethod, identifierField, path)
    }

    def jsObjectPathReference(obj: io.flow.postman.generator.attributes.v0.models.PathReference): play.api.libs.json.JsObject = {
      play.api.libs.json.Json.obj(
        "related_service_namespace" -> play.api.libs.json.JsString(obj.relatedServiceNamespace),
        "resource_type" -> play.api.libs.json.JsString(obj.resourceType),
        "operation_method" -> play.api.libs.json.JsString(obj.operationMethod.toString),
        "identifier_field" -> play.api.libs.json.JsString(obj.identifierField)
      ) ++ (obj.path match {
        case None => play.api.libs.json.Json.obj()
        case Some(x) => play.api.libs.json.Json.obj("path" -> play.api.libs.json.JsString(x))
      })
    }

    implicit def jsonWritesPostmanGeneratorAttributesPathReference: play.api.libs.json.Writes[PathReference] = {
      new play.api.libs.json.Writes[io.flow.postman.generator.attributes.v0.models.PathReference] {
        def writes(obj: io.flow.postman.generator.attributes.v0.models.PathReference) = {
          jsObjectPathReference(obj)
        }
      }
    }
  }
}

package io.flow.postman.generator.attributes.v0 {

  object Bindables {

    import play.api.mvc.{PathBindable, QueryStringBindable}

    // import models directly for backwards compatibility with prior versions of the generator
    import Core._
    import Models._

    object Core {
      implicit def pathBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.DateTime] = ApibuilderPathBindable(ApibuilderTypes.dateTimeIso8601)
      implicit def queryStringBindableDateTimeIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.DateTime] = ApibuilderQueryStringBindable(ApibuilderTypes.dateTimeIso8601)

      implicit def pathBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): PathBindable[_root_.org.joda.time.LocalDate] = ApibuilderPathBindable(ApibuilderTypes.dateIso8601)
      implicit def queryStringBindableDateIso8601(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[_root_.org.joda.time.LocalDate] = ApibuilderQueryStringBindable(ApibuilderTypes.dateIso8601)
    }

    object Models {
      import io.flow.postman.generator.attributes.v0.models._

      val attributeNameConverter: ApibuilderTypeConverter[io.flow.postman.generator.attributes.v0.models.AttributeName] = new ApibuilderTypeConverter[io.flow.postman.generator.attributes.v0.models.AttributeName] {
        override def convert(value: String): io.flow.postman.generator.attributes.v0.models.AttributeName = io.flow.postman.generator.attributes.v0.models.AttributeName(value)
        override def convert(value: io.flow.postman.generator.attributes.v0.models.AttributeName): String = value.toString
        override def example: io.flow.postman.generator.attributes.v0.models.AttributeName = io.flow.postman.generator.attributes.v0.models.AttributeName.PostmanBasicAuth
        override def validValues: Seq[io.flow.postman.generator.attributes.v0.models.AttributeName] = io.flow.postman.generator.attributes.v0.models.AttributeName.all
      }
      implicit def pathBindableAttributeName(implicit stringBinder: QueryStringBindable[String]): PathBindable[io.flow.postman.generator.attributes.v0.models.AttributeName] = ApibuilderPathBindable(attributeNameConverter)
      implicit def queryStringBindableAttributeName(implicit stringBinder: QueryStringBindable[String]): QueryStringBindable[io.flow.postman.generator.attributes.v0.models.AttributeName] = ApibuilderQueryStringBindable(attributeNameConverter)
    }

    trait ApibuilderTypeConverter[T] {

      def convert(value: String): T

      def convert(value: T): String

      def example: T

      def validValues: Seq[T] = Nil

      def errorMessage(key: String, value: String, ex: java.lang.Exception): String = {
        val base = s"Invalid value '$value' for parameter '$key'. "
        validValues.toList match {
          case Nil => base + "Ex: " + convert(example)
          case values => base + ". Valid values are: " + values.mkString("'", "', '", "'")
        }
      }
    }

    object ApibuilderTypes {
      import org.joda.time.{format, DateTime, LocalDate}

      val dateTimeIso8601: ApibuilderTypeConverter[DateTime] = new ApibuilderTypeConverter[DateTime] {
        override def convert(value: String): DateTime = format.ISODateTimeFormat.dateTimeParser.parseDateTime(value)
        override def convert(value: DateTime): String = format.ISODateTimeFormat.dateTime.print(value)
        override def example: DateTime = DateTime.now
      }

      val dateIso8601: ApibuilderTypeConverter[LocalDate] = new ApibuilderTypeConverter[LocalDate] {
        override def convert(value: String): LocalDate = format.ISODateTimeFormat.yearMonthDay.parseLocalDate(value)
        override def convert(value: LocalDate): String = value.toString
        override def example: LocalDate = LocalDate.now
      }

    }

    final case class ApibuilderQueryStringBindable[T](
      converters: ApibuilderTypeConverter[T]
    ) extends QueryStringBindable[T] {

      override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, T]] = {
        params.getOrElse(key, Nil).headOption.map { v =>
          try {
            Right(
              converters.convert(v)
            )
          } catch {
            case ex: java.lang.Exception => Left(
              converters.errorMessage(key, v, ex)
            )
          }
        }
      }

      override def unbind(key: String, value: T): String = {
        s"$key=${converters.convert(value)}"
      }
    }

    final case class ApibuilderPathBindable[T](
      converters: ApibuilderTypeConverter[T]
    ) extends PathBindable[T] {

      override def bind(key: String, value: String): _root_.scala.Either[String, T] = {
        try {
          Right(
            converters.convert(value)
          )
        } catch {
          case ex: java.lang.Exception => Left(
            converters.errorMessage(key, value, ex)
          )
        }
      }

      override def unbind(key: String, value: T): String = {
        converters.convert(value)
      }
    }

  }

}
