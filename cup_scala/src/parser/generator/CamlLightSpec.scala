
package parser.generator

import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}
import edu.tum.cup2.grammar.Symbol

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
			AND, OR, NOT, // &, or, not
			TYPE, // type
			IF, THEN, ELSE, // if then else
			IN, OF, LET, REC // in of let rec
			= TerminalEnum
	}

	object NonTerminals extends SymbolEnum {
		val expr, const, typeexpr, typedef, pattern, expr_seq = NonTerminalEnum
	}

	import NonTerminals._
	import Terminals._

	val terminals = Terminals
	val nonTerminals = NonTerminals

	import parser.ast.expressions._
	import parser.ast.types._
	import parser.ast.patterns.Pattern

	class INTCONST extends SymbolValue[Int]
	class IDENTIFIER extends SymbolValue[String]
	class expr extends SymbolValue[Expression]
	class const extends SymbolValue[Const]
	class typeexpr extends SymbolValue[TypeExpression]
	class typedef extends SymbolValue[TypeDefinition]
	class pattern extends SymbolValue[Pattern]
	class expr_seq extends SymbolValue[List[Expression]]

	precedences(left(MUL), left(SEMI))

	grammar(
		expr -> (
			IDENTIFIER ^^ { (str: String) => Id(str) } |
			INTCONST ^^ { (n: Int) => Integer(n) } |
			LBRACKET ~ expr ~ RBRACKET ^^ { (expr: Expression) => expr } |
			LSQBRACKET ~ expr ~ RSQBRACKET ^^ { (seq: Expression) => ListExpression.fromExpression(seq) } |
			chain(SEMI, Sequence(_, _)) |
			binOp(MUL, BinaryOperator.mul) |
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ { (cond: Expression, ifTrue: Expression, ifFalse: Expression) => IfThenElse(cond, ifTrue, ifFalse) }
		)
	)

	def chain(terminal: Symbol, action: (Expression, Expression) => Expression) =
		(expr ~ terminal ~ expr) ^^ { (expr1: Expression, expr2: Expression) => action(expr1, expr2) }

	def binOp(terminal: Symbol, op: BinaryOperator.Value) =
		chain(terminal, BinOp(op, _, _))

	def unOp(terminal: Symbol, op: UnaryOperator.Value) =
		(terminal ~ expr) ^^ { (expr: Expression) => UnOp(op, expr) }

}
