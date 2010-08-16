
package parser.generator

import parser.ast.expressions.Expression
import Normalizer.normalize

import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.io.LRParsingTableDump
import edu.tum.cup2.parser.LRParser
import java.io.StringReader
import scala.io.Source
import scala.collection.mutable.ListBuffer

object Test extends Application {

	// example code from <http://www2.in.tum.de/~petter/cup2/#3.3.1.>
	val generator = new LR1Generator(CamlLightSpec)
	val table = generator.getParsingTable()
	val parser = new LRParser(table)

	LRParsingTableDump.dumpToHTML(table, new java.io.File("build/dump.htm"))

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
					CamlLightSpec.resetCounter()
					Left(normalize(parser.parse(new CamlLightScanner(new StringReader(input))).asInstanceOf[Expression]).toString);
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
