
package parser.generator

import parser.ast.expressions.Expression
import Normalizer.normalize

import java.io.StringReader
import scala.io.Source
import scala.collection.mutable.ListBuffer

object Test extends Application {

	var i = 0
	val failed = ListBuffer[Int]()
	val ignored = ListBuffer[Int]()
	for (line <- Source.fromFile("test/parser/generator/testcases").getLines()) {
		if (line.startsWith("#")) {
			ignored += i
		}
		else {
			val Array(input, output) = line.split('#')

			println("TEST "+i+":")
			println("input = " + input)

			val result =
				try {
					Left(normalize(CamlLightSpec.parse(new StringReader(input))._1).toString);
				}
				catch {
					case ex => Right(ex)
				}

			if (output == "!") {
				println("expected exception")
				if (result.isLeft) {
					println("*** FAIL   " + result.left.get)
					failed += i
				}
				else {
					println("*** WIN")
				}
			}
			else {
				println("expected = " + output)
				result match {
					case Left(`output`) => println("*** WIN")
					case Left(res) =>
						println("*** FAIL   " + res)
						failed += i
					case Right(ex) =>
						println("*** FAIL   " + ex)
						failed += i
				}
			}
		}

		i += 1
	}

	if (!failed.isEmpty)
		println("FAILED TESTS: " + failed.mkString(", "))
	if (!ignored.isEmpty)
		println("IGNORED TESTS: " + ignored.mkString(", "))
	
}
