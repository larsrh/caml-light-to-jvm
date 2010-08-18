
package tests

import parser.ast.expressions._
import parser.ast.types._
import parser.generator.Normalizer
import parser.generator.CamlLightSpec

import scala.io.Source

import java.io.StringReader

class ParserTests extends TestSuite {

	// SIMPLE TESTCASES (without type definitions)
	private def parse(input: String) = CamlLightSpec.parse(new StringReader(input))

	for (line <- Source.fromFile("test/parser/generator/simple-testcases").getLines()) {
		val ignore = line.startsWith("#")
		val l = if (ignore) line.substring(1) else line
		val Array(input, output) = l.split('#')
		if (ignore)
			test(alwaysIgnore(input))
		else {
			val tester = calculate(parse(input))

			if (output == "!")
				test(tester.shouldThrow[Exception])
			else
				test(tester.shouldSuffice { case (e, _) =>
					Normalizer.normalize(e).toString == output
				})
		}
	}

	// COMPLEX TESTCASES (with type definition)
	private def checkExpression(expected: Expression)(input: CamlLightSpec.Program) = {
		assertEquals(expected, input._1)
	}

	private def checkTypeDefinition(expected: List[TypeDefinition])(input: CamlLightSpec.Program) = {
		assertEquals(expected, input._2)
	}

	test(
		calculate(parse("type 'a foo = Bar of 'a * 'a;; 3")).all(
			checkExpression(Integer(3)),
			checkTypeDefinition(List(Data("foo", List(TypeVariable(0)),("Bar", Some(TypeTuple(TypeVariable(0), TypeVariable(0)))))))
		)
	)

}
