
package parser.generator

import edu.tum.cup2.generator.LALR1Generator
import edu.tum.cup2.parser.LRParser
import java.io.StringReader
import scala.io.Source
import scala.collection.mutable.ListBuffer

object Test extends Application {

	// example code from <http://www2.in.tum.de/~petter/cup2/#3.3.1.>
	val generator = new LALR1Generator(CamlLightSpec.instance)
	val table = generator.getParsingTable()
	val parser = new LRParser(table)

	var i = 0
	val failed = ListBuffer[Int]()
	for (line <- Source.fromFile("test/testcases").getLines()) {
		val Array(input, output) = line.split('#')
		val result = parser.parse(new CamlLightScanner(new StringReader(input)));

		println("TEST "+i+":")
		println("input = " + input)
		println("expected = " + output)
		if (result.toString != output) {
			println("*** FAIL   " + result)
			failed += i
		}
		else {
			println("*** WIN")
		}

		i += 1
	}

	if (!failed.isEmpty)
		println("FAILED TESTS: " + failed.mkString(", "))
	
}
