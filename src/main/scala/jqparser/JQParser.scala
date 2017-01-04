package jqparser

import fastparse.WhitespaceApi
import fastparse.noApi._
import fastparse.core.Logger

object JQParser {

  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharPred(_.isWhitespace).rep)
  }

  import White._

  implicit val logger = Logger(System.err.println)

  val ident: P[String] = {
    // TODO add !keyword ~
    P((CharPred(_.isLetter) | "_") ~~ (CharPred(_.isLetterOrDigit) | "_").repX).!
  }

  val field: P[String] =
    P("." ~~ (CharPred(_.isLetter) | "_") ~~ (CharPred(_.isLetterOrDigit) | "_").repX).!

  val space = P(CharPred(_.isWhitespace).rep(min = 1))

  val string: P[String] = {
    val hexDigit = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
    val unicodeEscape = P("u" ~~ hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit)
    val escape = P("\\" ~~ (CharIn("\"/\\bfnrt") | unicodeEscape | "(" ~ exp ~ ")"))

    val strChars = P(CharsWhile(!"\"\\".contains(_)))
    P("\"" ~~ (strChars | escape).repX ~~ "\"").!
  }

  val literal: P[String] = {
    val digits = P(CharsWhile(_.isDigit))
    val exponent = P(CharIn("eE") ~~ CharIn("+-").? ~~ digits)
    val fractional = P("." ~~ digits)
    val integral = P("0" | CharIn('1' to '9') ~~ digits.?)

    P(CharIn("+-").? ~~ integral ~~ fractional.? ~~ exponent.?).!
  }

  val exp1: P[String] = P(
    "reduce" ~ term ~ "as" ~ pattern ~ "(" ~ exp ~ ";" ~ exp ~ ")" |
      "foreach" ~ term ~ "as" ~ pattern ~ "(" ~ exp ~ ";" ~ exp ~ (";" ~ exp).? ~ ")" |
      "label" ~ varRef ~ "|" ~ exp |
      "try" ~ exp ~ ("catch" ~ exp).? |
      "if" ~ exp ~ "then" ~ exp ~ elseBody |
      funDef ~ exp |
      term ~ "as" ~ pattern ~ "|" ~ exp |
      term

  ).!

  val exp2: P[String] = P(
    exp1 ~ "?" |
      exp1 |
      "-" ~ exp
  ).!

  val exp3: P[String] = P(
    exp2.rep(min = 1, sep = P(
      "and" |
        "or" |
        "//=" |
        "|=" |
        ">=" |
        "==" |
        "<=" |
        "+=" |
        "%=" |
        "/=" |
        "//" |
        "*=" |
        "!=" |
        "-=" |
        "|" |
        ">" |
        "=" |
        "<" |
        "+" |
        "%" |
        "/" |
        "*" |
        "," |
        "-"
    ))
  ).!

  val exp: P[String] = exp3

  val elseBody: P[String] = P(
    "elif" ~ exp ~ "then" ~ exp ~ elseBody |
      "else" ~ exp ~ "end"
  ).!

  val arrayIndexer =
    P("[" ~ (exp.? ~ (":" ~ exp.?).? | exp ~ ":") ~ "]" ~~ "?".?)
  val objectIndexer =
    P((field | "." ~~ string) ~~ "?".?)

  val varRef =
    P("$" ~~ ident).!

  val term1: P[String] = P(
    "break" ~~ space ~~ varRef |
      objectIndexer |
      format |
      "$" ~~ "__loc__" |
      varRef |
      ".." |
      "." |
      "{" ~ MkDict ~ "}" |
      "(" ~ exp ~ ")" |
      "[" ~ exp.? ~ "]" |
      ident ~ "(" ~ exp.rep(min = 1, sep = ";") ~ ")" |
      literal |
      string |
      ident

  ).!

  val term2: P[String] = P(
    term1 ~~ P(arrayIndexer | objectIndexer).repX(min = 1)
  ).!

  val term: P[String] = term2 | term1

  val MkDict: P[String] = P(
    MkDictPair.rep(sep = ",")
  ).!

  val MkDictPair: P[String] = P(
    (keyword | ident | string) ~ ":" ~ ExpD |
      string |
      varRef |
      ident |
      "(" ~ exp ~ ")" ~ ":" ~ ExpD
  ).!

  val ExpD1: P[String] = P(
    term |
      "-" ~ ExpD
  ).!

  val ExpD2: P[String] = P(
    ExpD1.rep(min = 2, sep = P("|"))
  ).!

  val ExpD: P[String] = P(
    ExpD2 | ExpD1
  ).!

  val keyword: P[String] = P(
    "utf8bytelength" |
      "__loc__" |
      "foreach" |
      "include" |
      "import" |
      "module" |
      "reduce" |
      "break" |
      "catch" |
      "label" |
      "elif" |
      "else" |
      "then" |
      "and" |
      "def" |
      "end" |
      "try" |
      "as" |
      "if" |
      "or"
  ).!

  val pattern: P[String] = P(
    varRef |
      "[" ~ pattern.rep(min = 1, sep = ",") ~ "]" |
      "{" ~ objectPattern.rep(min = 1, sep = ",") ~ "}"
  ).!


  val objectPattern: P[String] = P(
    varRef |
      ident ~ ":" ~ pattern |
      keyword ~ ":" ~ pattern |
      string ~ ":" ~ pattern |
      "(" ~ exp ~ ")" ~ ":" ~ pattern
  ).!

  val format: P[String] = P(
    "@" ~~ ident ~~ (space ~ string).?
  ).!

  val funDef: P[String] = P(
    "def" ~~ space ~ ident ~ ("(" ~ params ~ ")").? ~ ":" ~ exp ~ ";"
  ).!

  val params: P[String] = P(
    (varRef | ident).rep(min = 1, sep = ";")
  ).!

  val jq: P[String] =
    P(Start ~ exp ~ End)

  def parse(input: String) = jq.parse(input)
}
