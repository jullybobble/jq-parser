package jqparser

import fastparse.core.Parsed.Success
import org.specs2.mutable.Specification

class JQParserSpec extends Specification {

  def testParse(input: String) = {
    s"parse sucessfully '$input'" in {
      val result = JQParser.parse(input)
      result must beAnInstanceOf[Success[_, _, _]]
    }
  }

  "JQ Parser" should {
    testParse(".")

    testParse(".foo")

    testParse(".[\"foo\"]")

    testParse(".foo?")

    testParse(".[0]")

    testParse(".[2]")

    testParse(".[2:4]")

    testParse(".[:3]")

    testParse(".[-2:]")

    testParse(".[-2]")

    testParse(".[]")

    testParse(".foo, .bar")

    testParse(".user, .projects[]")

    testParse(".[4,2]")

    testParse(".[] | .name")

    testParse("[.user, .projects[]]")

    testParse("{user, title: .titles[]}")

    testParse("{(.user): .titles}")

    testParse(".a + 1")

    testParse(".a + .b")

    testParse(".a + null")

    testParse(".a + 1")

    testParse("{a: 1} + {b: 2} + {c: 3} + {a: 42}")

    testParse("4 - .a")

    testParse(". - [\"xml\", \"yaml\"]")

    testParse("10 / . * 3")

    testParse(". / \", \"")

    testParse("{\"k\": {\"a\": 1, \"b\": 2}} * {\"k\": {\"a\": 0,\"c\": 3}}")

    testParse(".[] | (1 / .)?")

    testParse(".[] | length")

    testParse("utf8bytelength")

    testParse("keys")

    testParse("keys")

    testParse("map(has(\"foo\"))")

    testParse("map(has(2))")

    testParse(".[] | in({\"foo\": 42})")

    testParse("map(in([0,1]))")

    testParse("map(.+1)")

    testParse("map_values(.+1)")

    testParse("path(.a[0].b)")

    testParse("[path(..)]")

    testParse("del(.foo)")

    testParse("del(.[1, 2])")

    testParse("getpath([\"a\",\"b\"])")

    testParse("[getpath([\"a\",\"b\"], [\"a\",\"c\"])]")

    testParse("setpath([\"a\",\"b\"]; 1)")

    testParse("setpath([\"a\",\"b\"]; 1)")

    testParse("setpath([0,\"a\"]; 1)")

    testParse("to_entries")

    testParse("from_entries")

    testParse("with_entries(.key |= \"KEY_\" + .)")

    testParse("map(select(. >= 2))")

    testParse(".[] | select(.id == \"second\")")

    testParse(".[]|numbers")

    testParse("1, empty, 2")

    testParse("[1,2,empty,3]")

    testParse("try error(\"\\($__loc__)\") catch .")

    testParse("[paths]")

    testParse("[paths(scalars)]")

    testParse("add")

    testParse("any")

    testParse("all")

    testParse("flatten")

    testParse("flatten(1)")

    testParse("flatten")

    testParse("flatten")

    testParse("range(2;4)")

    testParse("[range(2;4)]")

    testParse("[range(4)]")

    testParse("[range(0;10;3)]")

    testParse("[range(0;10;-1)]")

    testParse("[range(0;-5;-1)]")

    testParse("floor")

    testParse("sqrt")

    testParse(".[] | tonumber")

    testParse(".[] | tostring")

    testParse("map(type)")

    testParse(".[] | (infinite * .) < 0")

    testParse("infinite, nan | type")

    testParse("sort")

    testParse("sort_by(.foo)")

    testParse("group_by(.foo)")

    testParse("min")

    testParse("max_by(.foo)")

    testParse("unique")

    testParse("unique_by(.foo)")

    testParse("unique_by(length)")

    testParse("reverse")

    testParse("contains(\"bar\")")

    testParse("contains([\"baz\", \"bar\"])")

    testParse("contains([\"bazzzzz\", \"bar\"])")

    testParse("contains({foo: 12, bar: [{barp: 12}]})")

    testParse("contains({foo: 12, bar: [{barp: 15}]})")

    testParse("indices(\", \")")

    testParse("indices(1)")

    testParse("indices([1,2])")

    testParse("index(\", \")")

    testParse("rindex(\", \")")

    testParse("inside(\"foobar\")")

    testParse("inside([\"foobar\", \"foobaz\", \"blarp\"])")

    testParse("inside([\"foobar\", \"foobaz\", \"blarp\"])")

    testParse("inside({\"foo\": 12, \"bar\":[1,2,{\"barp\":12, \"blip\":13}]})")

    testParse("inside({\"foo\": 12, \"bar\":[1,2,{\"barp\":12, \"blip\":13}]})")

    testParse("[.[]|startswith(\"foo\")]")

    testParse("[.[]|endswith(\"foo\")]")

    testParse("combinations")

    testParse("combinations(2)")

    testParse("[.[]|ltrimstr(\"foo\")]")

    testParse("[.[]|rtrimstr(\"foo\")]")

    testParse("explode")

    testParse("implode")

    testParse("split(\", \")")

    testParse("join(\", \")")

    testParse("join(\" \")")

    testParse("[while(.<100; .*2)]")

    testParse("[.,1]|until(.[0] < 1; [.[0] - 1, .[1] * .[0]])|.[1]")

    testParse("recurse(.foo[])")

    testParse("recurse")

    testParse("recurse(. * .; . < 20)")

    testParse("walk(if type == \"array\" then sort else . end)")

    testParse("walk( if type == \"object\" then with_entries( .key |= sub( \"^_+\"; \"\") ) else . end )")

    testParse("..|.a?")

    testParse("env.PAGER")

    testParse("transpose")

    testParse("bsearch(0)")

    testParse("bsearch(0)")

    testParse("bsearch(4) as $ix | if $ix < 0 then .[-(1+$ix)] = 4 else . end")

    testParse("\"The input was \\(.), which is one less than \\(.+1)\"")

    testParse("[.[]|tostring]")

    testParse("[.[]|tojson]")

    testParse("[.[]|tojson|fromjson]")

    testParse("@html")

    testParse("@sh \"echo \\(.)\"")

    testParse("fromdate")

    testParse("strptime(\"%Y-%m-%dT%H:%M:%SZ\")")

    testParse("strptime(\"%Y-%m-%dT%H:%M:%SZ\")|mktime")

    testParse(".[] == 1")

    testParse(". < 5")

    testParse("42 and \"a string\"")

    testParse("(true, false) or false")

    testParse("(true, true) and (true, false)")

    testParse("[true, false | not]")

    testParse(".foo // 42")

    testParse(".foo // 42")

    testParse("try .a catch \". is not an object\"")

    testParse("[.[]|try .a]")

    testParse("try error(\"some exception\") catch .")

    testParse("[.[]|(.a)?]")

    testParse("test(\"foo\")")

    testParse(".[] | test(\"a b c # spaces are ignored\"; \"ix\")")

    testParse("match(\"(abc)+\"; \"g\")")

    testParse("match(\"foo\")")

    testParse("match([\"foo\", \"ig\"])")

    testParse("match(\"foo (?<bar123>bar)? foo\"; \"ig\")")

    testParse("[ match(\".\"; \"g\")] | length")

    testParse("capture(\"(?<a>[a-z]+)-(?<n>[0-9]+)\")")

    testParse(".bar as $x | .foo | . + $x")

    testParse(". as $i|[(.*2|. as $i| $i), $i]")

    testParse(". as [$a, $b, {c: $c}] | $a + $b + $c")

    testParse(".[] as [$a, $b] | {a: $a, b: $b}")

    testParse("def addvalue(f): . + [f]; map(addvalue(.[0]))")

    testParse("def addvalue(f): f as $x | map(. + $x); addvalue(.[0])")

    testParse("reduce .[] as $item (0; . + $item)")

    testParse("[limit(3;.[])]")

    testParse("[first(range(.)), last(range(.)), nth(./2; range(.))]")

    testParse("[range(.)]|[first, last, nth(5)]")

    testParse("[foreach .[] as $item ([[],[]]; if $item == null then [[],.[0]] else [(.[0] + [$item]),[]] end; if $item == null then .[1] else empty end)]")

    testParse("def range(init; upto; by): def _range: if (by > 0 and . < upto) or (by < 0 and . > upto) then ., ((.+by)|_range) else . end; if by == 0 then init else init|_range end | select((by > 0 and . < upto) or (by < 0 and . > upto)); range(0; 10; 3)")

    testParse("def while(cond; update): def _while: if cond then ., (update | _while) else empty end; _while; [while(.<100; .*2)]")

    testParse("[1|truncate_stream([[0],1],[[1,0],2],[[1,0]],[[1]])]")

    testParse("fromstream(1|truncate_stream([[0],1],[[1,0],2],[[1,0]],[[1]]))")

    testParse(". as $dot|fromstream($dot|tostream)|.==$dot")

    testParse("(..|select(type==\"boolean\")) |= if . then 1 else 0 end")

    testParse(".foo += 1")

    testParse("label $out | reduce .[] as $item (null; if .==false then break $out else $item.a end)")


  }
}
