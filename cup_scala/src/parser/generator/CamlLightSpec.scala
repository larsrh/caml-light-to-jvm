
package parser.generator

import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}
import edu.tum.cup2.grammar.Symbol
import edu.tum.cup2.spec.util.RHSItem
import scala.collection.mutable.ListBuffer

object CamlLightSpec {

	val instance = new CamlLightSpec()

}

class CamlLightSpec extends CUP2Specification with ScalaCUPSpecification {

	object Terminals extends SymbolEnum {
		val	IDENTIFIER,
			INTCONST,
			LBRACKET, RBRACKET, LSQBRACKET, RSQBRACKET, LBRACE, RBRACE, // ( ) [ ] { }
			MUL, PLUS, MINUS, DIV, CONS, SEMI, POINT, COMMA, // * + - / :: ; . ,
			LESS, LEQ, GREATER, GEQ, EQ, NEQ, BIND, // < <= > >= == <> =
			FUN, FUNCTION, MATCH, PIPE, // fun function match |
			LETAND, AND, OR, NOT, // and, &, or, not
			TYPE, // type
			IF, THEN, ELSE, // if then else
			IN, OF, LET, REC, // in of let rec
                        TRUE, FALSE,
                        STRING
			= TerminalEnum
	}

	object NonTerminals extends SymbolEnum {
		val expr, const, typeexpr, typedef, pattern, binding, andbindings = NonTerminalEnum
	}

	import NonTerminals._
	import Terminals._

	val terminals = Terminals
	val nonTerminals = NonTerminals

	import parser.ast.expressions._
	import parser.ast.types._
	import parser.ast.patterns.Pattern

	type Definition = (Pattern, Expression)

	class INTCONST extends SymbolValue[Int]
	class IDENTIFIER extends SymbolValue[String]
	class expr extends SymbolValue[Expression]
	class const extends SymbolValue[Const]
	class typeexpr extends SymbolValue[TypeExpression]
	class typedef extends SymbolValue[TypeDefinition]
	class pattern extends SymbolValue[Pattern]
	class binding extends SymbolValue[Definition]
	class andbindings extends SymbolValue[List[Definition]]

	precedences(left(MUL), left(DIV), left(PLUS), left(MINUS), left(CONS), left(EQ), left(LEQ), left(NEQ), left(GEQ), left(GREATER), left(LESS), left(AND), left(OR), left(COMMA), left(SEMI))

	/*ealed trait Expression
	final case class Bool(value: Boolean) extends Const
	final case class Character(value: Char) extends Const
	final case class BinOp(op: BinaryOperator.Value, expr1: Expression, expr2: Expression) extends Expression
	final case class UnOp(op: UnaryOperator.Value, expr: Expression) extends Expression
	final case class App(func: Expression, param: Expression*) extends Expression
	final case class Tuple(exprs: Expression*) extends Expression
	final case class Record(defs: (Id, Expression)*) extends Expression
	final case class Field(record: Expression, name: Id) extends Expression
	final case class Match(scrutinee: Expression, clauses: (Pattern, Expression)*) extends Expression
	final case class Lambda(body: Pattern, arguments: Pattern*) extends Expression*/

	val opMapping = Map[Operator#Value, Symbol](
		BinaryOperator.add -> PLUS,
		BinaryOperator.sub -> MINUS,
		BinaryOperator.mul -> MUL,
		BinaryOperator.div -> DIV,
		BinaryOperator.eq -> EQ,
		BinaryOperator.neq -> NEQ,
		BinaryOperator.geq -> GEQ,
		BinaryOperator.leq -> LEQ,
		BinaryOperator.gr -> GREATER,
		BinaryOperator.le -> LESS,
		BinaryOperator.and -> AND,
		BinaryOperator.or -> OR,
		UnaryOperator.neg -> NOT
	)

	grammar(
		expr -> (
			IDENTIFIER ^^ (Id.apply _) |
			INTCONST ^^ (Integer.apply _) |
			LBRACKET ~ expr ~ RBRACKET ^^ { (expr: Expression) => expr } |
			LSQBRACKET ~ expr ~ RSQBRACKET ^^ (ListExpression.fromExpression(_: Expression)) |
			chain(SEMI, Sequence(_, _)) |
			// ops
			{
				val buf = ListBuffer[RHSItem]()
				BinaryOperator.values.foreach { op => buf.append(binOp(op) : _*) }
				UnaryOperator.values.foreach  { op => buf.append(unOp(op) : _*) }
				buf.toSeq
			}
			|
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ (IfThenElse.apply _) |
			LET ~ pattern ~ BIND ~ expr ~ IN ~ expr ^^ (Let.apply _) |
			LET ~ REC ~ andbindings ~ IN ~ expr ^^ { (bindings: List[Definition], body: Expression) => LetRec(body, bindings: _*) }
		),
		binding -> (
			pattern ~ BIND ~ expr ^^ { (pattern: Pattern, expr: Expression) => (pattern, expr) }
		),
		andbindings -> (
			binding ^^ { (d: Definition) => List(d) } |
			binding ~ AND ~ andbindings ^^ { (head: Definition, tail: List[Definition]) => head :: tail }
		)
	)

	def chain(terminal: Symbol, action: (Expression, Expression) => Expression) =
		(expr ~ terminal ~ expr) ^^ { (expr1: Expression, expr2: Expression) => action(expr1, expr2) }

	def binOp(op: BinaryOperator.Value) = 
		chain(opMapping(op), BinOp(op, _, _))

	def unOp(op: UnaryOperator.Value) =
		(opMapping(op) ~ expr) ^^ { (expr: Expression) => UnOp(op, expr) }

}
