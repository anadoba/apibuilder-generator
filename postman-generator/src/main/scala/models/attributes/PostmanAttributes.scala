package models.attributes

import play.api.libs.json.Json

object PostmanAttributes {

  val BasicAuthKey = "postman-basic-auth"
  val SetupKey= "postman-organization-setup"

  case class PostmanBasicAuthAttrValue(
    username: String,
    password: String
  )

  implicit val postmanBasicAuthAttrValueFormat = Json.format[PostmanBasicAuthAttrValue]
}
