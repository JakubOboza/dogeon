package dogeon

import java.io.FileReader
import scala.util.parsing.combinator._

/*

    such "foo" is "bar" wow    // {"foo": "bar"}
    such "foo" is so "bar" next "baz" next "fizzbuzz" many wow    // {"foo": ["bar", "baz", "fizzbuzz"]}
    such "foo" is 42very3 wow    // {"foo": 42e3}

*/

class DogeonParser extends JavaTokenParsers {

  def value  : Parser[Any]  =  obj | arr | deguotedString | floatingPointNumber ^^ (_.toDouble) | "nullish" ^^ (n => null)| "nottrue" ^^ (p => false) | "notfalse" ^^ (p => true)

  def obj    : Parser[Map[String,Any]]  =  "such"~repsep(member, "wow")~"wow" ^^ { case "such"~ms~"wow" => Map() ++ ms}

  def arr    : Parser[List[Any]]  = "so"~>repsep(value, "next")<~"many"

  def member : Parser[(String, Any)]  = deguotedString~"is"~value ^^ { case name~"is"~value => (name, value)}

  def deguotedString: Parser[String] = stringLiteral ^^ {str => str.substring(1, str.length - 1)}

}


object Dogeon extends DogeonParser {

  def muchWow(dogeString: String) = parseAll(value, dogeString) match {
    case Success(result, in) => result
    case _ => "Much sad so bad"
  }

  // def main(args: Array[String]){
  //   val inputFile = "example.doge"
  //   val reader = new FileReader(inputFile)
  //   println(parseAll(value, reader))
  // }

}