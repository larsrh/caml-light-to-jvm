
package parser

import parser.ast.expressions.Expression
import parser.ast.types.TypeDefinition
import parser.generator._

import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.parser.LRParser
import edu.tum.cup2.parser.exceptions.MissingErrorRecoveryException

import java.io.Reader
import java.io.StringReader

object Position {
	private[parser] def fromScanner(scanner: CamlLightScanner) = Position(scanner.getLine, scanner.getColumn, scanner.yytext)
}

final case class Position(line: Int, column: Int, token: String) {
	override def toString = "line: %d, column: %d, token: %s".format(line, column, token)
}

sealed class CompilerException(msg: String, pos: Position, cause: Exception) extends Exception(msg, cause) {
	override def toString = "%s\nat %s".format(super.toString, pos.toString)
}

final case class ScannerException(pos: Position, cause: Exception) extends CompilerException("Scanning failed", pos, cause)
final case class ParserException(pos: Position, cause: Exception) extends CompilerException("Parsing failed", pos, cause)

object Parser {

	type Program = (Expression, List[TypeDefinition])

	private val lrparser =
		try {
			new LRParser(new LR1Generator(CamlLightSpec).getParsingTable())
		}
		catch {
			case ex: Exception =>
				throw new AssertionError("Initialisation of parser failed", ex)
		}

	def parse(string: String): Program = parse(new StringReader(string))

	def parse(reader: Reader) = {
		val scanner = new CamlLightScanner(reader)
		try {
			CamlLightSpec.reset()
			val result = lrparser.parse(scanner)
			assert(classOf[Program].isAssignableFrom(result.getClass))
			val (expr, types) = result.asInstanceOf[Program]
			(Normalizer.normalize(expr), types)
		}
		catch {
			case ex: MissingErrorRecoveryException =>
				throw new ParserException(Position.fromScanner(scanner), ex)
			case ex: IllegalArgumentException => // most probably from scanner
				throw new ScannerException(Position.fromScanner(scanner), ex)
		}
	}

}
