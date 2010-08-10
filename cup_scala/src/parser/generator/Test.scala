
package parser.generator

import edu.tum.cup2.generator.LALR1Generator
import edu.tum.cup2.parser.LRParser
import java.io.StringReader

object Test extends Application {

	// example code from <http://www2.in.tum.de/~petter/cup2/#3.3.1.>
	val generator = new LALR1Generator(CamlLightSpec.instance)
	val table = generator.getParsingTable()
	val parser = new LRParser(table)

	val result = parser.parse(new CamlLightScanner(new StringReader("[3*foo;4]")))

	println(result)

}
