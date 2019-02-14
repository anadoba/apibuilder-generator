package generator

import examples.ExampleJson
import org.scalatest.{Matchers, WordSpec}

class ServiceImportResolverSpec extends WordSpec with Matchers {

  import TestFixtures._

  "ServiceImportResolver" should {

    "return the same service untouched if importedServices array is empty" in new TrivialServiceContext {
      val result = ServiceImportResolver.resolveService(trivialService, Seq.empty)

      result.service shouldEqual trivialService
    }

    "return a service with empty imports list" in new ResolvedServiceTestContext {
      result.service.imports.isEmpty shouldEqual true
    }

    "return a service, which enums, models and unions count equal the used services components sum" in new ResolvedServiceTestContext {
      resultService.enums.length shouldEqual mainService.enums.length + importedService.enums.length
      resultService.models.length shouldEqual mainService.models.length + importedService.models.length
      resultService.unions.length shouldEqual mainService.unions.length + importedService.unions.length
    }

    "group all resources into one map" in new ResolvedServiceTestContext {
      resultService.resources shouldEqual mainService.resources
      result.serviceNamespaceToResources shouldEqual Map(
        mainService.namespace -> mainService.resources,
        importedService.namespace -> importedService.resources
      )
    }

    "leave the enums, models, unions type intact if it comes from the main service" in new ResolvedServiceTestContext {
      val enumsIntact = mainService.enums.forall(enum => resultService.enums.exists(_.name == enum.name))
      enumsIntact shouldEqual true

      val modelsIntact = mainService.models.forall(model => resultService.models.exists(_.name == model.name))
      modelsIntact shouldEqual true

      val unionsIntact = mainService.unions.forall(union => resultService.unions.exists(_.name == union.name))
      unionsIntact shouldEqual true
    }

    "rename the merged enums, models, unions type to the full namespace+name" in new ResolvedServiceTestContext {
      val enumNamesWithFullNamespace = importedService.enums.forall { enum =>
        resultService.enums.exists(e => e.name.contains(importedService.namespace) && e.name.contains(enum.name))
      }
      enumNamesWithFullNamespace shouldEqual true

      val modelNamesWithFullNamespace = importedService.models.forall { model =>
        resultService.models.exists(m => m.name.contains(importedService.namespace) && m.name.contains(model.name))
      }
      modelNamesWithFullNamespace shouldEqual true

      val unionNamesWithFullNamespace = importedService.unions.forall { union =>
        resultService.unions.exists(u => u.name.contains(importedService.namespace) && u.name.contains(union.name))
      }
      unionNamesWithFullNamespace shouldEqual true
    }

    "rename complex field types in the imported models so they match the full namespace+type" in new ResolvedServiceTestContext {
      val fieldNameToTypeMap = importedService.models.flatMap(_.fields).map(f => (f.name, f.`type`)).toMap
      val fieldNamesToCheck = fieldNameToTypeMap.filter {
        case (_, typ) => importedService.models.map(_.name).exists(n => typ.contains(n))
      }.keys.toList

      val updatedFieldsToTest = resultService.models.flatMap(_.fields)
        .filter(f => fieldNamesToCheck.contains(f.name))

      updatedFieldsToTest.foreach { field =>
        field.`type` should include(importedService.namespace)
      }
    }

    "prepare models in a way, which enables ExampleJson to prepare a sample JSON for every single one of them" in new ResolvedServiceTestContext {
      val exampleProvider = ExampleJson.allFields(resultService)
      resultService.models.foreach { model =>
        val exampleProvided = exampleProvider.sample(model.name).isDefined
        assert(exampleProvided, s"// it should provide an example for ${model.name}")
      }
    }

    "rename complex types in the imported models so they match the full namespace+type" in new ResolvedServiceWithUnionsTestContext {
      val unionNameToTypesMap = importedService.unions.map(u => (u.name, u.types)).toMap
      val typesDefinedInService = importedService.models.map(_.name) ++ importedService.enums.map(_.name)
      val unionNameToTypesMapToCheck = unionNameToTypesMap.map {
        case (name, types) =>
          val filteredTypes = types.filter(typ => typesDefinedInService.exists(n => typ.`type`.contains(n)))
          (name, filteredTypes)
      }
      val updatedUnionTypesToTest = resultService.unions
        .filter(u => unionNameToTypesMapToCheck.keys.exists(k => u.name.contains(k)))
        .flatMap { union =>
          union.types.filter { typ =>
            unionNameToTypesMapToCheck.values.flatten.exists(u => typ.`type`.contains(u.`type`))
          }
        }

      updatedUnionTypesToTest.foreach { typ =>
        typ.`type` should include(importedService.namespace)
      }
    }

    "prepare unions in a way, which enables ExampleJson to prepare a sample JSON for every single one of them" in new ResolvedServiceWithUnionsTestContext {
      val exampleProvider = ExampleJson.allFields(resultService)
      resultService.unions.foreach { union =>
        val exampleProvided = exampleProvider.sample(union.name).isDefined
        assert(exampleProvided, s"// it should provide an example for ${union.name}")
      }
    }
  }

  trait ResolvedServiceTestContext extends TrivialServiceWithImportCtx {
    val mainService = trivialServiceWithImport
    val importedService = models.TestHelper.referenceApiService

    lazy val result = ServiceImportResolver.resolveService(mainService, Seq(importedService))
    lazy val resultService = result.service
  }

  trait ResolvedServiceWithUnionsTestContext extends TrivialServiceWithUnionTypesImportCtx {
    val mainService = trivialServiceWithUnionTypesImport
    val importedService = models.TestHelper.generatorApiServiceWithUnionWithoutDescriminator

    lazy val result = ServiceImportResolver.resolveService(mainService, Seq(importedService))
    lazy val resultService = result.service
  }

}
