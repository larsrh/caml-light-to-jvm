
package tests

import parser._
import parser.ast.expressions._
import parser.ast.types._
import Parser.Program

import scala.io.Source

class ParserTests extends TestSuite {

	// SIMPLE TESTCASES (without type definitions)
	private def parse(input: String) = Parser.parse(input)

	for (line <- Source.fromFile("test/parser/generator/simple-testcases").getLines()) {
		val ignore = line.startsWith("#")
		val l = if (ignore) line.substring(1) else line
		val Array(input, output) = l.split('#')
		test(input,
			if (ignore)
				alwaysIgnore(input)
			else {
				lazy val tester = parse(input)

				if (output == "!")
					tester.shouldThrow[Exception]
				else
					tester.mapValue(_._1.toString).shouldBe(output)
			}
		)
	}

	// COMPLEX TESTCASES (with type definition)
	private def checkExpression(expected: Expression)(input: Program) = {
		assertEquals(expected, input._1)
	}

	private def checkTypeDefinition(expected: List[TypeDefinition])(input: Program) = {
		assertEquals(expected, input._2)
	}

	test(
		parse("type 'a foo = Bar of 'a * 'a;; 3").all(
			checkExpression(Integer(3)),
			checkTypeDefinition(List(Data("foo", List(TypeVariable(0)),("Bar", Some(TypeTuple(TypeVariable(0), TypeVariable(0)))))))
		)
	)

	// EXCEPTION TESTING
	test(parse("§").shouldThrow[ScannerException])
	test(parse("1/+").shouldThrow[ParserException])

}
