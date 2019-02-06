package generator

import io.flow.chuck.onboarding.v0.{models => postman}

object Utils {

  object Description {
    def apply(s: String): postman.Description = {
      postman.Description(content = Some(s), `type` = None)
    }
  }

  object Variable {
    def apply(
      key: String,
      value: String,
      `type`: String,
      id: _root_.scala.Option[String] = None,
      name: _root_.scala.Option[String] = None,
      description: _root_.scala.Option[io.flow.chuck.onboarding.v0.models.Description] = None,
      system: _root_.scala.Option[Boolean] = None,
      disabled: _root_.scala.Option[Boolean] = None
    ): postman.Variable = {
      postman.Variable(
        id,
        Some(key),
        Some(value),
        Some(`type`),
        name,
        description,
        system,
        disabled
      )
    }
  }

}
