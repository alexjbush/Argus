package argus.schema

import argus.json.JsonDiff
import cats.syntax.either._
import io.circe._
import org.scalatest._
import argus.macros._

import scala.io.Source

class SchemaSpec extends FlatSpec with Matchers with ASTMatchers {
  import Schema._

  def diffs(parsed: Root, original: String) = JsonDiff.diff(parsed.toJson, parser.parse(original).toOption.get)

  "Schema" should "round-trip a simple Json schema" in {
    val jsonStr = Source.fromInputStream(getClass.getResourceAsStream("/simple.json")).getLines.mkString("\n")
    val schema = Schema.fromResource("/simple.json")

    schema shouldBe a [Root]
    diffs(schema, jsonStr) shouldBe empty
  }
//https://github.com/scala/scala/blob/2.12.x/src/reflect/scala/reflect/internal/Printers.scala#L545
  it should "properly escape bad type names" in {

    val fieldsname = "`Address<(teststring|null)>`"
    val json =
      s"""
        |{
        |        "definitions": {
        |          "$fieldsname": {
        |            "type": "object",
        |            "additionalProperties": false,
        |            "properties": {
        |              "number": { "type": "integer" },
        |              "street": { "type": "string" }
        |            }
        |          },
        |          "SSN": { "type": "string" }
        |        },
        |        "type": "object",
        |        "additionalProperties": false,
        |        "properties": {
        |
        |          "name": { "type": "string" },
        |          "address": { "$$ref": "#/definitions/$fieldsname" },
        |          "ssn": { "$$ref": "#/definitions/SSN" }
        |        },
        |        "required" : ["name"]
        |      }
      """.stripMargin
    val base = schemaFromFields(
      Field("name",    schemaFromRef("#/definitions/Name")) ::
        Field("address", schemaFromRef(s"#/definitions/$fieldsname")) ::
        Field("id",      schemaFromSimpleType(SimpleTypes.Integer)) ::
        Nil
    )
    val defs = Schema.fromJson(json)
    import runtimeUniverse._
    val mb = new ModelBuilder[runtimeUniverse.type](runtimeUniverse)
    import argus.schema.Schema._
    val (typ: Tree, res) = mb.mkSchemaDef("Root", schema=base.copy(definitions = defs.definitions), "Foo" :: Nil)
    val code = res.map(showCode(_)).mkString("\n")
    println(code)
  }

  it should "decode enum's into a list of Json entries" in {
    val json =
      """
        |{
        |  "type" : "object",
        |  "properties": {
        |    "country": {
        |      "enum" : ["USA", 4, { "a" : "b" }]
        |     }
        |  }
        |}
      """.stripMargin
    val schema = Schema.fromJson(json)

    val countryEnum = for {
      props <- schema.properties
      country <- props.find(_.name == "country")
      enum <- country.schema.enum
    } yield enum

    countryEnum should === (Some(List( "\"USA\"", "4", """{"a":"b"}""" )))
  }

  it should "round trip more complex schemas" in {
//    val jsonStr = Source.fromInputStream(getClass.getResourceAsStream("/meta-schema.json")).getLines.mkString("\n")
//    val schema = Schema.fromJson(jsonStr)
//
//    schema shouldBe a [Schema.Root]
//    diffs(schema, jsonStr) shouldBe empty

    pending
  }

  it should "decode int64 format" in {
    val json =
      """
        |{
        |  "type" : "object",
        |  "properties": {
        |    "age": {
        |      "type": "integer",
        |      "format": "int64"
        |     }
        |  }
        |}
      """.stripMargin
    val schema = Schema.fromJson(json)

    val ageFormat = for {
      props <- schema.properties
      country <- props.find(_.name == "age")
      enum <- country.schema.format
    } yield enum

    ageFormat should === (Some(Formats.Int64))
  }

  it should "decode array of simple types format" in {
    val json =
      """
        |{
        |  "type" : "object",
        |  "properties": {
        |    "age": {
        |      "type": ["integer", "string"]
        |     }
        |  }
        |}
      """.stripMargin
    val schema = Schema.fromJson(json)

    val ageTyp = for {
      props <- schema.properties
      age <- props.find(_.name == "age")
      typ <- age.schema.typ
    } yield typ

    ageTyp should === (Some(ListSimpleTypeTyp(List(SimpleTypes.Integer, SimpleTypes.String))))
  }

  it should "parse an object with only additionalProperties set" in {
    // TODO work with ref?
    val json =
      """
        |{
        |  "additionalProperties": {
        |     "type": "integer"
        |   },
        |   "type": "object"
        |}
      """.stripMargin

    val schema = Schema.fromJson(json)
    val additionalPropsFormat = for {
      addProps <- schema.additionalProperties
      props <- addProps.toOption
    } yield props.typ
    additionalPropsFormat should be (Some(Some(SimpleTypeTyp(SimpleTypes.Integer))))
  }


  it should "decode unknown format" in {
    val json =
      """
        |{
        |  "type" : "object",
        |  "properties": {
        |    "age": {
        |      "type": "integer",
        |      "format": "color"
        |     }
        |  }
        |}
      """.stripMargin
    val schema = Schema.fromJson(json)

    val ageFormat = for {
      props <- schema.properties
      country <- props.find(_.name == "age")
      enum <- country.schema.format
    } yield enum

    ageFormat should === (Some(Formats.Unknown))
  }

}
