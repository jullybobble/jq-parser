# JQ-Parser

A parser for the the [JQ](https://stedolan.github.io/jq/) JSON processing language written with the 
[fastparse](http://www.lihaoyi.com/fastparse) library.

The parser was initially based on the grammar available in /fadado/jq-learn/blob/master/docs/grammar.txt, and the tests 
are based on the examples in the [JQ v1.5 Manual](https://stedolan.github.io/jq/manual/v1.5/). 

Currently the parser produces only the consumes input, can thus be used for validation only.

In the future we will produce an AST as well.

## Usage

To run the tests, run `sbt test` in the root of the project.