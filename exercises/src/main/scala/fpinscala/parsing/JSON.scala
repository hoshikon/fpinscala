package fpinscala.parsing

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces: Parser[String] = (char(' ')|char('\n')).many.slice
    def jInt: Parser[JNumber] = regex("-?(?:0|[1-9][0-9]*)".r).map(int => JNumber(int.toDouble))
    def jDouble: Parser[JNumber] = regex("-?(?:0|[1-9][0-9]*)\\.[0-9]+([e|E][+-]?[0-9]+)?".r).map(d => JNumber(d.toDouble))
    def jBool: Parser[JBool] = regex("(true|false)".r).map(bool => JBool(bool.toBoolean))
    def jArray: Parser[JArray] = map3(spaces ** string("[") ** spaces, jAnyIgnoreComma.many, spaces ** string("]") ** spaces)((_, array: List[JSON], _) => JArray(array.toIndexedSeq))
    def jString: Parser[JString] = regex("\"((?!\").)*\"".r).map(str => JString(str.tail.init))
    def jNull: Parser[JNull.type] = regex("null".r).map(_ => JNull)
    def jAny: Parser[JSON] = jDouble|jInt|jBool|jArray|jString|jNull|jObject
    def jAnyIgnoreComma: Parser[JSON] = map2(jAny, spaces ** string(",").many ** spaces)((a, _) => a)
    def keyAndValue: Parser[(String, JSON)] = map3(regex("\"((?!:).)*\"".r), spaces ** string(":") ** spaces, jAnyIgnoreComma)((k, _, v) => (k.tail.init, v))
    def jObject: Parser[JObject] = map3(string("{") ** spaces, keyAndValue.many1, spaces ** string("}"))((_, knv, _) => JObject(knv.toMap))

    map3(spaces, jObject, spaces)((_, ob, _) => ob)
  }
}
