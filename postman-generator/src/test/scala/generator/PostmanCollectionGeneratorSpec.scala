package generator

import io.apibuilder.generator.v0.models.{File, InvocationForm}
import io.apibuilder.spec.v0.models._
import org.scalatest.{Assertion, Matchers, WordSpec}
import play.api.libs.json.{JsArray, JsDefined, JsValue, Json}

import scala.util.Try

class PostmanCollectionGeneratorSpec extends WordSpec with Matchers {

  import models.TestHelper._

  "PostmanCollectionGenerator" should {

    "return an error when invocation form contains a service with declared, but not provided imports" in {
      val invocationForm = InvocationForm(referenceWithImportsApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isLeft shouldEqual true
      result.left.get shouldEqual Seq("Service imports need to be resolved before generating Postman Collection. However, InvocationForm.importedServices is empty")
    }

    "return a generated Postman Collection for a trivial service" in new TrivialServiceContext {
      val invocationForm = InvocationForm(trivialService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      result.isRight shouldEqual true

      val generatedCollectionJson = Json.parse(result.right.get.head.contents)
      // TODO: below JSON contains 'variable' and 'auth' elements that are hardcoded now. delete after fixing the logic
      generatedCollectionJson shouldEqual Json.parse(
        """
          |{
          |  "item" : [ {
          |    "item" : [ {
          |      "request" : {
          |        "method" : "POST",
          |        "header" : [ {
          |          "description" : {
          |            "content" : "Required to send JSON body"
          |          },
          |          "value" : "application/json",
          |          "key" : "Content-Type"
          |        } ],
          |        "body" : {
          |          "mode" : "raw",
          |          "raw" : "{\n  \"value\" : \"something\"\n}"
          |        },
          |        "url" : {
          |          "path" : [ "complex-strings", "new" ],
          |          "query" : [ ],
          |          "host" : [ "{{BASE_URL}}" ],
          |          "variable" : [ ],
          |          "raw" : "{{BASE_URL}}/complex-strings/new"
          |        }
          |      },
          |      "response" : [ ],
          |      "name" : "POST /complex-strings/new",
          |      "type" : "item"
          |    } ],
          |    "name" : "complex-strings",
          |    "type" : "folder"
          |  } ],
          |  "auth" : {
          |    "type" : "basic",
          |    "basic" : [ {
          |      "value" : "{{FLOW_TOKEN}}",
          |      "key" : "username"
          |    }, {
          |      "value" : "",
          |      "key" : "password"
          |    } ]
          |  },
          |  "variable" : [ {
          |    "type" : "string",
          |    "value" : "https://api.flow.io",
          |    "key" : "BASE_URL"
          |  } ],
          |  "event" : [ ],
          |  "info" : {
          |    "schema" : "https://schema.getpostman.com/json/collection/v2.1.0/collection.json",
          |    "name" : "trivial",
          |    "description" : { },
          |    "version" : "0.1"
          |  }
          |}
        """.stripMargin
      )
    }

    "return a generated Postman Collection for a reference service without imports" in {
      val invocationForm = InvocationForm(referenceApiService, importedServices = None)
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collectionJson =>
        val postmanFolders = (collectionJson \ "item").as[JsArray].value
        val folderNames = postmanFolders.map(js => (js \ "name").as[String])
        folderNames shouldEqual List("echoes", "groups", "members", "organizations", "users")

        val membersFolder = postmanFolders.find(js => (js \ "name").as[String] == "members").get
        val memberEndpoints = (membersFolder \ "item").as[JsArray].value
        val bulkCreateMemberEndpoint = memberEndpoints.find(js => (js \ "name").as[String].contains("/members/:organization/members_bulk"))
          .getOrElse(fail("reference service does not contain POST members_bulk operation"))
        val bulkCreateMemberRequest = bulkCreateMemberEndpoint \ "request"

        val bulkCreateMemberBody = bulkCreateMemberRequest \ "body"
        (bulkCreateMemberBody \ "mode").as[String] shouldEqual "raw"
        val bulkCreateMemberPayloadString = (bulkCreateMemberBody \ "raw").as[String]
        Try(Json.parse(bulkCreateMemberPayloadString)).isSuccess shouldEqual true

        val bulkCreateMemberRawUrl = (bulkCreateMemberRequest \ "url" \ "raw").as[String]
        bulkCreateMemberRawUrl shouldEqual "{{BASE_URL}}/members/:organization/members_bulk"

        val bulkCreateMemberUrlVariable = bulkCreateMemberRequest \ "url" \ "variable"
        bulkCreateMemberUrlVariable shouldEqual JsDefined(Json.parse(
          """
            |[{
            |    "key": "organization",
            |    "value": "{{ORGANIZATION}}",
            |    "description": {
            |      "content": "Type: uuid  | Required: true"
            |    },
            |    "disabled": false
            |}]
          """.stripMargin))

        (bulkCreateMemberRequest \ "method").as[String] shouldEqual "POST"
        val bulkCreateMemberHeader = (bulkCreateMemberRequest \ "header").as[JsArray].value.head
        (bulkCreateMemberHeader \ "key").as[String] shouldEqual "Content-Type"
        (bulkCreateMemberHeader \ "value").as[String] shouldEqual "application/json"
      }
    }

    "return a generated Postman Collection for a service with imports" in new TrivialServiceWithImportCtx {
      val invocationForm = InvocationForm(trivialServiceWithImport, importedServices = Some(Seq(referenceApiService)))
      val result = PostmanCollectionGenerator.invoke(invocationForm)

      assertResultCollectionJson(result) { collectionJson =>
        val postmanFolders = (collectionJson \ "item").as[JsArray].value
        val ageGroupsFolder = postmanFolders.find(js => (js \ "name").as[String] == "ages").get
        val ageGroupsEndpoints = (ageGroupsFolder \ "item").as[JsArray].value
        val getFirstAgeGroupEndpoint = ageGroupsEndpoints.find(js => (js \ "name").as[String].contains("/ages/first"))
          .getOrElse(fail("generated service does not contain GET /ages/first"))

        val getFirstAgeGroupEndpointResponseExample = (getFirstAgeGroupEndpoint \ "response").as[JsArray].value.head

        (getFirstAgeGroupEndpointResponseExample \ "code").as[Int] shouldEqual 200

        val responseExampleJson = Json.parse((getFirstAgeGroupEndpointResponseExample \ "body").as[String])
        val exampleGroup = (responseExampleJson \ "group").as[String]
        importedEnum.values.map(_.name) should contain(exampleGroup)
      }
    }

  }

  private def assertResultCollectionJson(result: Either[Seq[String], Seq[File]])(jsonAssertion: JsValue => Assertion): Assertion = {
    result.isRight shouldEqual true
    val resultFile = result.right.get.head
    resultFile.name.endsWith("postman_collection.json") shouldEqual true
    val postmanCollectionJson = Json.parse(resultFile.contents)

    jsonAssertion(postmanCollectionJson)
  }

  trait TrivialServiceContext {
    val trivialService = Service(
      apidoc = Apidoc("0.1"),
      name = "trivial",
      organization = Organization("test-org"),
      application = Application("test-app"),
      namespace = "io.trivial",
      version = "0.1",
      info = Info(license = None, contact = None),
      models = Seq(
        Model(
          name = "complex-string",
          plural = "complex-strings",
          fields = Seq(
            Field(
              name = "value",
              `type` = "string",
              example = Some("something"),
              required = true
            )
          ))
      ),
      resources = Seq(
        Resource(
          `type` = "complex-string",
          plural = "complex-strings",
          operations = Seq(
            Operation(
              method = Method.Post,
              path = "/complex-strings/new",
              body = Some(
                Body(`type` = "complex-string")
              )
            )
          )
        )
      )
    )

  }

  trait TrivialServiceWithImportCtx extends TrivialServiceContext {
    val importedEnum = referenceApiService.enums.find(_.name == "age_group")
      .getOrElse(fail("age_group enum is expected in example reference-service.json"))
    val importedEnumPath = s"${referenceApiService.namespace}.enums.age_group"

    val trivialServiceWithImport = trivialService.copy(
      imports = Seq(
        Import(
          uri = "some-uri",
          namespace = referenceApiService.name,
          organization = referenceApiService.organization,
          application = referenceApiService.application,
          version = referenceApiService.version,
          enums = Seq("age_group")
        )
      ),
      models = trivialService.models :+ Model(
        name = "age",
        plural = "ages",
        fields = Seq(
          Field(
            name = "group",
            `type` = importedEnumPath,
            required = true
          )
        )
      ),
      resources = trivialService.resources :+ Resource(
        `type` = "age",
        plural = "ages",
        operations = Seq(
          Operation(
            method = Method.Get,
            path = "/ages/first",
            responses = Seq(
              Response(
                code = ResponseCodeInt(200),
                `type` = "age"
              )
            )
          )
        )
      )
    )

  }

}

