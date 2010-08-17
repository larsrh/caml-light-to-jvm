
package tests

import parser.generator.Normalizer
import parser.generator.CamlLightSpec
import parser.ast.expressions.Expression
import parser.ast.types.TypeDefinition

import scala.io.Source

import java.io.StringReader

object ParserTests extends Tests {

	private def parse(input: String) = CamlLightSpec.parse(new StringReader(input))

	def parseExpression(input: String, expr: String) =
		calculate(parse(input)).shouldSuffice { case (e, _) =>
			Normalizer.normalize(e).toString == expr
		}

	// SIMPLE TESTCASES (without type definitions)
	for (line <- Source.fromFile("test/parser/generator/simple-testcases").getLines()) {
		val ignore = line.startsWith("#")
		val l = if (ignore) line.substring(1) else line
		val Array(input, output) = l.split('#')
		if (ignore)
			test(alwaysIgnore(input))
		else if (output == "!")
			test(parseExpression(input, output).shouldThrow[Exception])
		else
			test(parseExpression(input, output))
	}


}
