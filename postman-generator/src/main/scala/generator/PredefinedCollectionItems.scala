package generator

import generator.Heuristics.PathVariable
import io.flow.postman.v0.models._
import io.flow.postman.v0.{models => postman}

object PredefinedCollectionItems {

  def addItemTests(item: postman.Item): postman.Item = {

    val method = item.request.method.getOrElse("").toString.toUpperCase
    if (Seq("GET", "PUT", "POST").contains(method)) {
      val test = PredefinedCollectionItems.testEventResponseStatusOk(
        f"$method requests should return 2xx"
      )
      item.copy(event = Option(item.event.toSeq.flatten :+ test))
    } else {
      item
    }
  }

  def testEventResponseStatusOk(testTitle: String): Event = {
    Event(
      EventType.Test,
      Some(Script(Seq(
        s"""pm.test("$testTitle", function () {""",
        """    pm.response.to.be.success;""",
        """});"""
      )))
    )
  }

  def testPostStatusOk(testTitle: String, pathVariable: Option[PathVariable]): Event = {
    Event(
      EventType.Test,
      Some(Script {
        val test = Seq(
          s"""pm.test("$testTitle", function () {""",
          """    pm.response.to.be.success;""",
          """});"""
        )

        val pathVariableSetup = pathVariable.map { pathVar =>
          Seq(
            """var jsonData = JSON.parse(responseBody);""",
            s"""var id = jsonData["${pathVar.name}"];""",
            s"""if (id != null) pm.environment.set("${pathVar.postmanVarName}", id);"""
          )
        }.getOrElse(Seq.empty)

        test ++ pathVariableSetup
      }
    ))
  }
}