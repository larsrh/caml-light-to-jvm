
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
		INTCONST, BOOLCONST, STRINGCONST, CHARCONST,
		LBRACKET, RBRACKET, LSQBRACKET, RSQBRACKET, LBRACE, RBRACE, // ( ) [ ] { }
		STAR, PLUS, MINUS, SLASH, CONS, SEMI, POINT, COMMA, // * + - / :: ; . ,
		LESS, LEQ, GREATER, GEQ, EQ, NEQ, BIND, // < <= > >= == <> =
		FUN, FUNCTION, MATCH, PIPE, ARROW, UNDERSCORE, WITH, // fun function match | -> _ with
		LETAND, AND, OR, NOT, // and, &, or, not
		TYPE, // type
		IF, THEN, ELSE, // if then else
		IN, OF, LET, REC // in of let rec
		= TerminalEnum
}

object CamlLightSpec extends CUP2Specification with ScalaCUPSpecification {

	object NonTerminals extends SymbolEnum {
		val expr, const, pattern, caselist, `case`, binding, andbindings, commaseq, entry, record, app = NonTerminalEnum
		val typeexpr, typedef, param, cdecl = NonTerminalEnum
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
	class BOOLCONST extends SymbolValue[Boolean]
	class STRINGCONST extends SymbolValue[String]
	class CHARCONST extends SymbolValue[Char]
	class IDENTIFIER extends SymbolValue[String]
	class expr extends SymbolValue[Expression]
	class const extends SymbolValue[Const]
	class pattern extends SymbolValue[Pattern]
	class `case` extends SymbolValue[Definition]
	class caselist extends SymbolValue[List[Definition]]
	class binding extends SymbolValue[Definition]
	class andbindings extends SymbolValue[List[Definition]]
	class commaseq extends SymbolValue[List[Expression]]
	class entry extends SymbolValue[Entry]
	class record extends SymbolValue[List[Entry]]
	class app extends SymbolValue[List[Expression]]
	class typeexpr extends SymbolValue[TypeExpression]
	class typedef extends SymbolValue[TypeDefinition]

	precedences(left(POINT), left(STAR), left(SLASH), left(PLUS), left(MINUS), left(CONS), left(EQ), left(LEQ), left(NEQ), left(GEQ), left(GREATER), left(LESS), left(AND), left(OR), left(BIND), left(COMMA), left(IF), left(THEN), left(ELSE), left(SEMI), left(PIPE), left(LET), left(REC), left(IN), left(FUN), left(FUNCTION), left(MATCH), left(WITH))

	// TODO app, match, lambda
	// TODO fix letrec

	val opMapping = Map[Operator#Value, Symbol](
		BinaryOperator.add -> PLUS,
		BinaryOperator.sub -> MINUS,
		BinaryOperator.mul -> STAR,
		BinaryOperator.div -> SLASH,
		BinaryOperator.eq -> EQ,
		BinaryOperator.neq -> NEQ,
		BinaryOperator.geq -> GEQ,
		BinaryOperator.leq -> LEQ,
		BinaryOperator.gr -> GREATER,
		BinaryOperator.le -> LESS,
		BinaryOperator.and -> AND,
		BinaryOperator.or -> OR,
		UnaryOperator.not -> NOT,
		UnaryOperator.neg -> MINUS
	)

	grammar(
		expr -> (
			IDENTIFIER ^^ (Id.apply _) |
			INTCONST ^^ (Integer.apply _) |
			BOOLCONST ^^ (Bool.apply _) |
			STRINGCONST ^^ { (str: String) => ListExpression.fromSeq(str.map(Character.apply _).toList) } |
			CHARCONST ^^ (Character.apply _) |
			//expr ~ app ^^ { (head: Expression, tail: List[Expression]) => App(head, tail: _*) } |
			LBRACKET ~ commaseq ~ RBRACKET ^^ { (seq: List[Expression]) => seq match {
				case List(expr) => expr
				case List(l @ _*) => Tuple(l: _*)
			} } |
			LSQBRACKET ~ expr ~ RSQBRACKET ^^ (ListExpression.fromExpression(_: Expression)) |
			chain(SEMI, Sequence(_, _)) |
			// ops
			{
				val buf = ListBuffer[RHSItem]()
				BinaryOperator.values.foreach { op => buf ++= chain(opMapping(op), BinOp(op, _, _)) }
				UnaryOperator.values.foreach  { op => buf ++= (opMapping(op) ~ expr) ^^ (UnOp(op, _: Expression)) }
				buf.toSeq
			} |
			MATCH ~ expr ~ WITH ~ caselist ^^ { (expr: Expression, cases: List[Definition]) => Match(expr, cases: _*) } |
			expr ~ POINT ~ IDENTIFIER ^^ { (expr: Expression, id: String) => Field(expr, Id(id)) } |
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ (IfThenElse.apply _) |
			LET ~ pattern ~ BIND ~ expr ~ IN ~ expr ^^ (Let.apply _) |
			LET ~ REC ~ andbindings ~ IN ~ expr ^^ { (bindings: List[Definition], body: Expression) => LetRec(body, bindings: _*) } |
			LBRACE ~ record ~ RBRACE ^^ { (entries: List[Entry]) => expressions.Record(entries: _*) }
		),
		binding -> (
			pattern ~ BIND ~ expr ^^ { (pattern: Pattern, expr: Expression) => (pattern, expr) }
		),
		andbindings -> (
			binding ^^ { (d: Definition) => List(d) } |
			binding ~ AND ~ andbindings ^^ { (head: Definition, tail: List[Definition]) => head :: tail }
		),
		commaseq -> (
			expr ^^ { (expr: Expression) => List(expr) } |
			expr ~ COMMA ~ commaseq ^^ { (head: Expression, tail: List[Expression]) => head :: tail }
		),
		entry -> (
			IDENTIFIER ~ BIND ~ expr ^^ { (id: String, expr: Expression) => (Id(id), expr) }
		),
		record -> (
			entry ^^ { (e: Entry) => List(e) } |
			entry ~ SEMI ~ record ^^ { (head: Entry, tail: List[Entry]) => head :: tail }
		),
		/*app -> (
			expr ^^ { (expr: Expression) => List(expr) } |
			expr ~ app ^^ { (head: Expression, tail: List[Expression]) => head :: tail }
		),*/
		pattern -> (
			IDENTIFIER ^^ (patterns.Id.apply _) |
			UNDERSCORE ^^ { () => patterns.Underscore }
			// TODO more cases
		),
		`case` -> (
			pattern ~ ARROW ~ expr ^^ { (pattern: Pattern, expr: Expression) => (pattern, expr) }
		),
		caselist -> (
			`case` ^^ { (c: Definition) => List(c) } |
			`case` ~ PIPE ~ caselist ^^ { (head: Definition, tail: List[Definition]) => head :: tail }
		)

		/*,

		typedef -> (
			TYPE ~ param ~ IDENTIFIER ~ BIND ~ cdecl ^^ { (param: List[TypeVariable], id: String, cdecl: List[TypeExpression]) => val a = cdecl match {
				case List(x) => x
				case List(l @ _*) => TypeConstructor("Tuple", l: _*)
			}
			val b = param match {
				case List(id) => id
				case List(l @ _*) => TypeConstructor("Tuple", l: _*)
			}
			Data.apply(id, a, b)
			}
		),

		param -> (
			IDIENTIFIER ^^ { x: String => (Id(x), None) }
		),

		cdecl -> (
			IDENTIFIER ^^ { x: String => (Id(x), None) }
		)*/
	)

	def chain(terminal: Symbol, action: (Expression, Expression) => Expression) =
		(expr ~ terminal ~ expr) ^^ { (expr1: Expression, expr2: Expression) => action(expr1, expr2) }

}
