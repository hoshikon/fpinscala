package fpinscala.parsing

import fpinscala.SimpleBooleanTest
import fpinscala.parsing.JSON._
import fpinscala.parsing.MyParser._

object ParsersTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val jsonParser: Parser[JSON] = JSON.jsonParser(MyParsers)
    val input =
      "{" +
      "  \"name\" : \"Hoshikon\",\n" +
      "  \"age\" : 31,\n" +
      "  \"height\" : 165.5,\n" +
      "  \"married\" : true,\n" +
      "  \"friends\" : [\n" +
      "    {\n" +
      "        \"name\" : \"Eugene\",\n" +
      "        \"age\" : 27\n" +
      "    },\n" +
      "    {\n"  +
      "        \"name\" : \"Greg\",\n" +
      "        \"age\" : 24\n" +
      "    }\n" +
      "  ]\n" +
      "}"

    val result: Either[ParseError, JSON] = MyParsers.run(jsonParser)(input)
//    result match {
//      case Right(a) => println(a)
//      case Left(err) => println(err)
//    }

    val expected = JObject(
      Map(
        "name" -> JString("Hoshikon"),
        "height" -> JNumber(165.5),
        "age" -> JNumber(31.0),
        "married" -> JBool(true),
        "friends" -> JArray(Vector(
          JObject(Map("name" -> JString("Eugene"), "age" -> JNumber(27.0))),
          JObject(Map("name" -> JString("Greg"), "age" -> JNumber(24.0)))
        ))
      )
    )

    val parserTest = result == Right(expected)
    printTest(parserTest, "json parser")
  }

  run
}
