
package parser

import parser.ast.expressions.Expression
import parser.ast.types.TypeDefinition
import parser.generator._

import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.io.LRParserSerialization
import edu.tum.cup2.parser.LRParser
import edu.tum.cup2.parser.exceptions.MissingErrorRecoveryException
import edu.tum.cup2.semantics.ActionCallback

import scala.collection.mutable
import scala.util.control.Breaks._

import java.io.File
import java.io.FileNotFoundException
import java.io.Reader
import java.io.StringReader

object Position {
	private[parser] def fromScanner(scanner: CamlLightScanner) = Position(scanner.getLine, scanner.getColumn, scanner.yytext)
}

final case class Position(line: Int, column: Int, token: String) {
	override def toString = "line: %d, column: %d, token: %s".format(line, column, token)
}

final class BootstrapException(msg: String, cause: Exception) extends Exception(msg, cause)

sealed class CompilerException(msg: String, pos: Position, cause: Exception) extends Exception(msg, cause) {
	override def toString = "%s\nat %s".format(super.toString, pos.toString)
}

final class ScannerException(pos: Position, cause: Exception) extends CompilerException("Scanning failed", pos, cause)
final class ParserException(pos: Position, cause: Exception) extends CompilerException("Parsing failed", pos, cause)

object Parser {

	// called by ant task 'cup2-compile'
	def main(args: Array[String]) {
		new LRParserSerialization(args(0)).saveParser(lrparser)
	}

	object Program {
		type Parent = (Expression, List[TypeDefinition])
		def unapply(prog: Program) = Tuple2.unapply(prog)
	}

	final class Program(val expr: Expression, val typeDefs: List[TypeDefinition], _positions: mutable.Map[Expression, Position])
		extends Program.Parent(expr, typeDefs) {

		// convert to immutable Map
		val positions = Map[Expression, Position]() ++ _positions
	}

	private def loadParserFromFile(filename: String, `throw`: Boolean = true): Option[LRParser] = {
		try {
			if (new File(filename).exists)
				Some(new LRParserSerialization(filename).loadParser())
			else
				throw new FileNotFoundException(filename)
		}
		catch {
			case ex: Exception =>
				if (`throw`)
					throw new BootstrapException("Failed to load parsing table at location %s".format(filename), ex)
				else
					None
		}
		
	}

	private def mkCallback(scanner: CamlLightScanner, map: mutable.Map[Expression, Position]) = new ActionCallback {
		override def actionDone(input: Object): Object = {
			input match {
				case expr: Expression =>
					map(expr) = Position.fromScanner(scanner)
				case _ =>
			}
			input
		}
	}

	private def normalizeAndReplace(map: mutable.Map[Expression, Position]): (Expression => Expression) = {
		val stack = mutable.Stack[Expression]()
		def normalize(expr: Expression): Expression = {
			import parser.ast.expressions._

			val push = map.contains(expr)
			if (push) stack.push(expr)

			val normalized = expr match {
				case Sequence(e1, e2) => Sequence(normalize(e1), normalize(e2))
				case IfThenElse(c, t, f) => IfThenElse(normalize(c), normalize(t), normalize(f))
				case Let(p, d, b) => Let(p, normalize(d), normalize(b))
				case LetRec(b, p @ _*) => LetRec(normalize(b), p.toList: _*)
				case BinOp(BinaryOperator.sub, Integer(0), e) => UnOp(UnaryOperator.neg, normalize(e))
				case BinOp(op, e1, e2) => BinOp(op, normalize(e1), normalize(e2))
				case App(App(func, head @ _*), tail @ _*) => normalize(App(func, (head ++ tail).toList: _*))
				case App(func, args @ _*) => App(normalize(func), args map normalize toList: _*)
				case Cons(head, tail) => Cons(normalize(head), normalize(tail))
				case Tuple(exprs @ _*) => Tuple(exprs map normalize toList: _*)
				case TupleElem(tuple, nr) => TupleElem(normalize(tuple), nr)
				case Record(defs @ _*) => Record(defs map { d => (d._1, normalize(d._2)) } toList: _*)
				case Field(rec, name) => Field(normalize(rec), name)
				case Match(scrutinee, clauses @ _*) => Match(normalize(scrutinee), clauses map { c => (c._1, normalize(c._2)) } toList: _*)
				case Lambda(body, pats @ _*) => Lambda(normalize(body), pats.toList: _*)
				case _ => expr
			}

			map.remove(expr) match {
				case Some(pos) => map(normalized) = pos
				case None if !stack.isEmpty => map(normalized) = map(stack.top)
				case _ =>
			}

			if (push) stack.pop()
			normalized
		}
		normalize
	}

	private val lrparser: LRParser =
		try {
			val sysLocation = System.getProperty("parser.table")
			if (sysLocation != null) {
				loadParserFromFile(sysLocation, true).get
			}
			else {
				val candidates = Stream("table.ser", "dist/table.ser", "build/table.ser")
				val available = candidates map { name => loadParserFromFile(name, false) }
				available.find(None !=) match {
					case None =>
						//println("Notice: Regenerating parser from source")
						new LRParser(new LR1Generator(CamlLightSpec).getParsingTable())
					case Some(p) => p.get
				}
			}
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
			val map = new mutable.HashMap[Expression, Position]()
			val result = lrparser.parse(scanner, mkCallback(scanner, map))
			assert(classOf[Program.Parent].isAssignableFrom(result.getClass))
			val (expr, types) = result.asInstanceOf[Program.Parent]
			val normalized = normalizeAndReplace(map)(expr)
			new Program(normalized, types, map)
		}
		catch {
			case ex: MissingErrorRecoveryException =>
				throw new ParserException(Position.fromScanner(scanner), ex)
			case ex: IllegalArgumentException => // most probably from scanner
				throw new ScannerException(Position.fromScanner(scanner), ex)
		}
	}

}
