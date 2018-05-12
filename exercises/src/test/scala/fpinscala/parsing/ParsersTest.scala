package fpinscala.parsing

import fpinscala.SimpleBooleanTest
import fpinscala.parsing.JSON._
import fpinscala.parsing.MyParser.Parser

object ParsersTest extends App with SimpleBooleanTest {
  override def run: Unit = {
    val jsonParser: Parser[JSON] = JSON.jsonParser(MyParser.MyParsers)
    val input =
      "{" +
        "\"name\" : \"Hoshikon\"," +
        "\"age\" : 31," +
        "\"height\" : 165.5," +
        "\"married\" : true" +
        "\"friends\" : [" +
          "{" +
              "\"name\" : \"Eugene\"," +
              "\"age\" : 27" +
          "}," +
          "{"  +
              "\"name\" : \"Greg\"," +
              "\"age\" : 24" +
          "}" +
        "]" +
        " }"

    val result: Either[ParseError, JSON] = MyParser.MyParsers.run(jsonParser)(input)

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
