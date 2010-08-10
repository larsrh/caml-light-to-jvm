
package parser.generator

import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}
import edu.tum.cup2.grammar.Symbol
import edu.tum.cup2.spec.util.RHSItem
import scala.collection.mutable.ListBuffer

// exported for usage in JFlex
object CamlLightTerminals extends SymbolEnum {
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

object CamlLightSpec extends CUP2Specification with ScalaCUPSpecification {

	object NonTerminals extends SymbolEnum {
		val expr, const, typeexpr, typedef, pattern, binding, andbindings, commaseq, entry, record = NonTerminalEnum
	}

	val terminals = CamlLightTerminals
	val nonTerminals = NonTerminals

	import terminals._
	import nonTerminals._

	import parser.ast._
	import expressions._
	import types._
	import patterns.Pattern

	type Definition = (Pattern, Expression)
	type Entry = (Id, Expression)

	class INTCONST extends SymbolValue[Int]
	class IDENTIFIER extends SymbolValue[String]
	class expr extends SymbolValue[Expression]
	class const extends SymbolValue[Const]
	class typeexpr extends SymbolValue[TypeExpression]
	class typedef extends SymbolValue[TypeDefinition]
	class pattern extends SymbolValue[Pattern]
	class binding extends SymbolValue[Definition]
	class andbindings extends SymbolValue[List[Definition]]
	class commaseq extends SymbolValue[List[Expression]]
	class entry extends SymbolValue[Entry]
	class record extends SymbolValue[List[Entry]]

	precedences(left(POINT), left(MUL), left(DIV), left(PLUS), left(MINUS), left(CONS), left(EQ), left(LEQ), left(NEQ), left(GEQ), left(GREATER), left(LESS), left(AND), left(OR), left(COMMA), left(SEMI))

	// TODO bool, character, app, tuple, string, match, lambda

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
		UnaryOperator.not -> NOT
	)

	grammar(
		expr -> (
			IDENTIFIER ^^ (Id.apply _) |
			MINUS ~ IDENTIFIER ^^ { (id: String) => UnOp(UnaryOperator.neg, Id(id)) } |
			INTCONST ^^ (Integer.apply _) |
			MINUS ~ INTCONST ^^ { (n: Int) => Integer(-n) } |
			LBRACKET ~ expr ~ RBRACKET ^^ { (expr: Expression) => expr } |
			LSQBRACKET ~ expr ~ RSQBRACKET ^^ (ListExpression.fromExpression(_: Expression)) |
			chain(SEMI, Sequence(_, _)) |
			// ops
			{
				val buf = ListBuffer[RHSItem]()
				BinaryOperator.values.foreach { op => buf.append(binOp(op) : _*) }
				UnaryOperator.values.foreach  { op => if (opMapping.contains(op)) buf.append(unOp(op) : _*) } // TODO
				buf.toSeq
			} |
			expr ~ POINT ~ IDENTIFIER ^^ { (expr: Expression, id: String) => Field(expr, Id(id)) } |
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ (IfThenElse.apply _) |
			LET ~ pattern ~ BIND ~ expr ~ IN ~ expr ^^ (Let.apply _) |
			LET ~ REC ~ andbindings ~ IN ~ expr ^^ { (bindings: List[Definition], body: Expression) => LetRec(body, bindings: _*) } |
			//commaseq ^^ { (seq: List[Expression]) => Tuple(seq: _*) }
			LBRACE ~ record ~ RBRACE ^^ { (entries: List[Entry]) => expressions.Record(entries: _*) }
		),
		binding -> (
			pattern ~ BIND ~ expr ^^ { (pattern: Pattern, expr: Expression) => (pattern, expr) }
		),
		andbindings -> (
			binding ^^ { (d: Definition) => List(d) } |
			binding ~ AND ~ andbindings ^^ { (head: Definition, tail: List[Definition]) => head :: tail }
		),
		/*commaseq -> (
			expr ^^ { (expr: Expression) => List(expr) } |
			expr ~ COMMA ~ commaseq ^^ { (head: Expression, tail: List[Expression]) => head :: tail }
		),*/
		entry -> (
			IDENTIFIER ~ BIND ~ expr ^^ { (id: String, expr: Expression) => (Id(id), expr) }
		),
		record -> (
			entry ^^ { (e: Entry) => List(e) } |
			entry ~ SEMI ~ record ^^ { (head: Entry, tail: List[Entry]) => head :: tail }
		)
	)

	def chain(terminal: Symbol, action: (Expression, Expression) => Expression) =
		(expr ~ terminal ~ expr) ^^ { (expr1: Expression, expr2: Expression) => action(expr1, expr2) }

	def binOp(op: BinaryOperator.Value) = 
		chain(opMapping(op), BinOp(op, _, _))

	def unOp(op: UnaryOperator.Value) =
		(opMapping(op) ~ expr) ^^ { (expr: Expression) => UnOp(op, expr) }

}
