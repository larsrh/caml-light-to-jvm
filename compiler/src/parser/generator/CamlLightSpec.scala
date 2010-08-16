
package parser.generator

import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}
import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.parser.LRParser
import edu.tum.cup2.grammar.Symbol

// exported for usage in JFlex
object CamlLightTerminals extends SymbolEnum {
	val	IDENTIFIER,
		INTCONST, BOOLCONST, STRINGCONST, CHARCONST,
		LBRACKET, RBRACKET, LSQBRACKET, RSQBRACKET, LBRACE, RBRACE, // ( ) [ ] { }
		STAR, PLUS, MINUS, SLASH, CONS, SEMI, SEMISEMI, POINT, COMMA, TUPLEACC, // * + - / :: ; ;; . , #
		LESS, LEQ, GREATER, GEQ, EQ, NEQ, BIND, // < <= > >= == <> =
		FUN, FUNCTION, MATCH, PIPE, ARROW, UNDERSCORE, WITH, // fun function match | -> _ with
		LETAND, AND, OR, NOT, // and, &, or, not
		TYPE, // type
		IF, THEN, ELSE, // if then else
		IN, OF, LET, REC, // in of let rec
		APP_DUMMY, NEG_DUMMY // used for %prec
		= TerminalEnum
}

object CamlLightSpec extends CUP2Specification with ScalaCUPSpecification {

	object NonTerminals extends SymbolEnum {
		val toplevel, statement = NonTerminalEnum
		val expr, simpleexpr, simpleexprlist, list, binding, andbindings, commaseq, entry, record = NonTerminalEnum
		val pattern, caselist, `case`, funcase, funcaselist, patentry, patrecord, pattuple = NonTerminalEnum
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

	type Case = (Pattern, Expression)
	type Definition = (patterns.Id, Expression)
	type Entry = (Id, Expression)
	type PatEntry = (patterns.Id, Pattern)
	type FunCase = (List[Pattern], Expression)
	type Statement = Either[Expression => Expression, TypeDefinition]
	type Program = (Expression, List[TypeDefinition])

	class INTCONST extends SymbolValue[Int]
	class BOOLCONST extends SymbolValue[Boolean]
	class STRINGCONST extends SymbolValue[String]
	class CHARCONST extends SymbolValue[Char]
	class IDENTIFIER extends SymbolValue[String]
	class toplevel extends SymbolValue[Program]
	class statement extends SymbolValue[Statement]
	class expr extends SymbolValue[Expression]
	class simpleexpr extends SymbolValue[Expression]
	class simpleexprlist extends SymbolValue[List[Expression]]
	class list extends SymbolValue[List[Expression]]
	class pattern extends SymbolValue[Pattern]
	class pattuple extends SymbolValue[List[Pattern]]
	class `case` extends SymbolValue[Case]
	class caselist extends SymbolValue[List[Case]]
	class funcase extends SymbolValue[FunCase]
	class funcaselist extends SymbolValue[List[FunCase]]
	class patentry extends SymbolValue[PatEntry]
	class patrecord extends SymbolValue[List[PatEntry]]
	class binding extends SymbolValue[Definition]
	class andbindings extends SymbolValue[List[Definition]]
	class commaseq extends SymbolValue[List[Expression]]
	class entry extends SymbolValue[Entry]
	class record extends SymbolValue[List[Entry]]
	class typeexpr extends SymbolValue[TypeExpression]
	class typedef extends SymbolValue[TypeDefinition]

	private var symCount = 0

	def freshSym() = {
		val sym = "0" + symCount
		symCount += 1
		sym
	}

	private lazy val lrparser = new LRParser(new LR1Generator(this).getParsingTable())

	def parse(reader: java.io.Reader) = {
		symCount = 0
		val result = lrparser.parse(new CamlLightScanner(reader))
		assert(classOf[Program].isAssignableFrom(result.getClass))
		result.asInstanceOf[Program]
	}

	precedences(
		left(POINT), left(APP_DUMMY), left(TUPLEACC),
		left(STAR, SLASH), left(PLUS, MINUS),
		left(NEG_DUMMY),
		right(CONS),
		left(EQ), left(LEQ), left(NEQ), left(GEQ), left(GREATER), left(LESS),
		left(NOT), left(AND), left(OR),
		left(BIND),
		left(COMMA),
		left(IF), left(THEN), left(ELSE),
		left(SEMI),
		left(PIPE), left(ARROW), left(LET), left(REC), left(LETAND), left(IN),
		left(FUN), left(FUNCTION),
		left(MATCH), left(WITH),
		left(SEMISEMI)
	)

	grammar(
		toplevel -> (
			expr ^^ { (expr: Expression) => (expr, List[TypeDefinition]()) } |
			expr ~ SEMISEMI ^^ { (expr: Expression) => (expr, List[TypeDefinition]()) } |
			statement ~ SEMISEMI ~ toplevel ^^ { (stmt: Statement, prog: Program) => stmt match {
				case Left(func) => (func(prog._1), prog._2)
				case Right(typedef) => (prog._1, typedef :: prog._2)
			} }
		),
		statement -> (
			LET ~ pattern ~ BIND ~ expr ^^ { (pat: Pattern, definition: Expression) => Left((expr: Expression) => Let(pat, definition, expr)) } |
			LET ~ REC ~ andbindings ^^ { (bindings: List[Definition]) => Left((expr: Expression) => LetRec(expr, bindings: _*)) } |
			typedef ^^ { (typedef: TypeDefinition) => Right(typedef) }
		),
		expr -> (
			simpleexpr ^^ (Predef.identity[Expression] _) |
			prec(rhs(simpleexpr, simpleexprlist), APP_DUMMY) ^^ { (head: Expression, tail: List[Expression]) => App(head, tail: _*) } |
			expr ~ TUPLEACC ~ INTCONST ^^ { (expr: Expression, n: Int) => TupleElem(expr, n) } |
			chain(CONS, Cons.apply) |
			//chain(SEMI, Sequence.apply) |
			// bin ops
			(Map(
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
				BinaryOperator.or -> OR
			) flatMap { ops => chain(ops._2, BinOp(ops._1, _, _)) } toSeq) |
			NOT ~ expr ^^ (UnOp(UnaryOperator.not, _: Expression)) |
			prec(rhs(MINUS, expr), NEG_DUMMY) ^^ (UnOp(UnaryOperator.neg, _: Expression)) |
			MATCH ~ expr ~ WITH ~ caselist ^^ { (expr: Expression, cases: List[Case]) => Match(expr, cases: _*) } |
			expr ~ POINT ~ IDENTIFIER ^^ { (expr: Expression, id: String) => Field(expr, Id(id)) } |
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ (IfThenElse.apply _) |
			LET ~ pattern ~ BIND ~ expr ~ IN ~ expr ^^ (Let.apply _) |
			LET ~ REC ~ andbindings ~ IN ~ expr ^^ { (bindings: List[Definition], body: Expression) => LetRec(body, bindings: _*) } |
			LBRACE ~ record ~ RBRACE ^^ { (entries: List[Entry]) => expressions.Record(entries: _*) } |
			FUNCTION ~ caselist ^^ { (cases: List[Case]) =>
				val sym = freshSym()
				Lambda(Match(Id(sym), cases: _*), List(patterns.Id(sym)): _*)
			} |
			FUN ~ funcaselist ^^ { (cases: List[FunCase]) =>
				val lengths = cases map (_._1.length) toSet;
				if (lengths.size != 1) throw new IllegalArgumentException("funcaselist")

				// we read input of form
				//   fun
				//     p11 p12 ... p1i -> e1
				//   | p21 p22 ... p2i -> e2
				//   | ...
				// and transform it to
				//   lambda x1 ... xi => match (x1, ..., xi) with
				//     (p11, p12, ..., p1i) -> e1
				//   | ...
				val syms = (1 to lengths.head) map { _ => freshSym() } toList
				val tupledCases = cases map { case (pats, expr) => (patterns.Tuple(pats: _*), expr) }
				Lambda(
					Match(Tuple(syms.map(Id.apply): _*), tupledCases: _*),
					syms.map(patterns.Id.apply): _*
				)
			}
		),
		simpleexpr -> (
			IDENTIFIER ^^ (Id.apply _) |
			INTCONST ^^ (Integer.apply _) |
			BOOLCONST ^^ (Bool.apply _) |
			STRINGCONST ^^ { (str: String) => ListExpression.fromSeq(str.map(Character.apply _).toList) } |
			CHARCONST ^^ (Character.apply _) |
			LBRACKET ~ commaseq ~ RBRACKET ^^ { (seq: List[Expression]) => seq match {
				case List(expr) => expr
				case List(l @ _*) => Tuple(l: _*)
			} } |
			LSQBRACKET ~ RSQBRACKET ^^ { () => expressions.Nil } |
			LSQBRACKET ~ list ~ RSQBRACKET ^^ { (seq: List[Expression]) => ListExpression.fromSeq(seq) }
		),
		simpleexprlist -> (
			simpleexpr ^^ { (expr: Expression) => List(expr) } |
			simpleexpr ~ simpleexprlist ^^ { (head: Expression, tail: List[Expression]) => head :: tail }
		),
		list -> (
			expr ^^ { (expr: Expression) => List(expr) } |
			expr ~ SEMI ~ list ^^ { (head: Expression, tail: List[Expression]) => head :: tail }
		),
		binding -> (
			IDENTIFIER ~ BIND ~ expr ^^ { (str: String, expr: Expression) => (patterns.Id(str), expr) }
		),
		andbindings -> (
			binding ^^ { (d: Definition) => List(d) } |
			binding ~ LETAND ~ andbindings ^^ { (head: Definition, tail: List[Definition]) => head :: tail }
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
		pattern -> (
			IDENTIFIER ^^ (patterns.Id.apply _) |
			UNDERSCORE ^^ { () => patterns.Underscore } |
			LBRACE ~ patrecord ~ RBRACE ^^ { (pats: List[PatEntry]) => patterns.Record(pats: _*) } |
			INTCONST ^^ (patterns.Integer.apply _) |
			BOOLCONST ^^ (patterns.Bool.apply _) |
			CHARCONST ^^ (patterns.Character.apply _) |
			STRINGCONST ^^ { (str: String) => patterns.ListPattern.fromSeq(str.map(patterns.Character.apply _).toList) } |
			LSQBRACKET ~ RSQBRACKET ^^ { () => patterns.Nil } |
			pattern ~ CONS ~ pattern ^^ { (pat1: Pattern, pat2: Pattern) => patterns.Cons(pat1, pat2) } |
			LBRACKET ~ pattuple ~ RBRACKET ^^ { (pats: List[Pattern]) => pats match {
				case List(pat) => pat
				case List(l @ _*) => patterns.Tuple(l: _*)
			} }
			// TODO string const
			// TODO more cases
		),
		pattuple -> (
			pattern ^^ { (pattern: Pattern) => List(pattern) } |
			pattern ~ COMMA ~ pattuple ^^ { (head: Pattern, tail: List[Pattern]) => head :: tail }
		),
		patentry -> (
			IDENTIFIER ~ BIND ~ pattern ^^ { (id: String, pat: Pattern) => (patterns.Id(id), pat) }
		),
		patrecord -> (
			patentry ^^ { (e: PatEntry) => List(e) } |
			patentry ~ SEMI ~ patrecord ^^ { (head: PatEntry, tail: List[PatEntry]) => head :: tail }
		),
		`case` -> (
			pattern ~ ARROW ~ expr ^^ { (pattern: Pattern, expr: Expression) => (pattern, expr) }
		),
		caselist -> (
			`case` ^^ { (c: Case) => List(c) } |
			`case` ~ PIPE ~ caselist ^^ { (head: Case, tail: List[Case]) => head :: tail }
		),
		funcase -> (
			pattern ~ ARROW ~ expr ^^ { (pattern: Pattern, expr: Expression) => (List(pattern), expr) } |
			pattern ~ funcase ^^ { (pattern: Pattern, funcase: FunCase) => (pattern :: funcase._1, funcase._2) }
		),
		funcaselist -> (
			funcase ^^ { (c: FunCase) => List(c) } |
			funcase ~ PIPE ~ funcaselist ^^ { (head: FunCase, tail: List[FunCase]) => head :: tail }
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
