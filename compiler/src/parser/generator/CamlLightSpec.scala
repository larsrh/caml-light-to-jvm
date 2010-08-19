
package parser.generator

import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}
import edu.tum.cup2.grammar.Symbol

import scala.collection.mutable.HashMap

// exported for usage in JFlex
object CamlLightTerminals extends SymbolEnum {
	val	IDENTIFIER,
		INTCONST, BOOLCONST, STRINGCONST, CHARCONST,
		LBRACKET, RBRACKET, LSQBRACKET, RSQBRACKET, LBRACE, RBRACE, // ( ) [ ] { }
		STAR, PLUS, MINUS, SLASH, CONS, SEMI, SEMISEMI, POINT, COMMA, TUPLEACC, // * + - / :: ; ;; . , #
		SQIDENTIFIER, // 'a ('sq' stands for 'single quote')
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
		val typeexpr, typeexpr2, typeexpr3, tupletypeexpr, typedef, typeheader, paramlist, cdecl, cdecllist = NonTerminalEnum
	}

	val terminals = CamlLightTerminals
	val nonTerminals = NonTerminals

	import terminals._
	import nonTerminals._

	import parser.ast._
	import expressions._
	import types._
	import patterns.Pattern

	private type Program = (Expression, List[TypeDefinition])
	private type Case = (Pattern, Expression)
	private type Definition = (patterns.Id, Expression)
	private type Entry = (Id, Expression)
	private type PatEntry = (patterns.Id, Pattern)
	private type FunCase = (List[Pattern], Expression)
	private type Statement = Either[Expression => Expression, TypeDefinition]
	private type CDecl = (String, Option[TypeExpression])

	class INTCONST extends SymbolValue[Int]
	class BOOLCONST extends SymbolValue[Boolean]
	class STRINGCONST extends SymbolValue[String]
	class CHARCONST extends SymbolValue[Char]
	class IDENTIFIER extends SymbolValue[String]
	class SQIDENTIFIER extends SymbolValue[String]
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
	class cdecl extends SymbolValue[CDecl]
	class cdecllist extends SymbolValue[List[CDecl]]
	class typeexpr extends SymbolValue[TypeExpression]
	class typeexpr2 extends SymbolValue[TypeExpression]
	class typeexpr3 extends SymbolValue[TypeExpression]
	class tupletypeexpr extends SymbolValue[List[TypeExpression]]
	class typedef extends SymbolValue[TypeDefinition]
	class typeheader extends SymbolValue[(String, List[TypeVariable])]
	class paramlist extends SymbolValue[List[TypeVariable]]

	/*
	 * Tracks usage of new variables introduced by lambda expressions
	 */
	private var varCount: Int = _

	/*
	 * The source code uses string literals for type variables, e. g.
	 * 'a, but the AST uses Ints internally, so we need a mapping.
	 */
	private val typeVars = HashMap[String, TypeVariable]()

	/*
	 * Tracks usage of new type variables
	 */
	private var tvCount: Int = _

	/*
	 * Generate a new variable, beginning with "0" so no collisions
	 * can occur.
	 */
	private def newVar() = {
		varCount += 1
		"0" + varCount
	}

	/*
	 * Generates a new mapping between a type variable and an unused
	 * AST representation of it, or return the existing representation, if any.
	 */
	private def getTypeVar(name: String) = {
		typeVars.getOrElseUpdate(name, { tvCount += 1; TypeVariable(tvCount) })
	}

	/*
	 * Resets this object to the initial state, causing all new variables
	 * starting with 0.
	 */
	private[parser] def reset() = {
		tvCount = -1
		varCount = -1
		typeVars.clear()
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
				val sym = newVar()
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
				val syms = (1 to lengths.head) map { _ => newVar() } toList
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
			STRINGCONST ^^ { (str: String) => Expression.fromSeq(str.map(Character(_)).toList) } |
			CHARCONST ^^ (Character.apply _) |
			LBRACKET ~ commaseq ~ RBRACKET ^^ { (seq: List[Expression]) => seq match {
				case List(expr) => expr
				case List(l @ _*) => Tuple(l: _*)
			} } |
			LSQBRACKET ~ RSQBRACKET ^^ { () => expressions.Nil } |
			LSQBRACKET ~ list ~ RSQBRACKET ^^ { (seq: List[Expression]) => Expression.fromSeq(seq) }
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
			STRINGCONST ^^ { (str: String) => patterns.Pattern.fromSeq(str.map(patterns.Character(_)).toList) } |
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
		),

		typeexpr -> (
			typeexpr2 ^^ (identity[TypeExpression] _) |
			typeexpr ~ ARROW ~ typeexpr2 ^^ { (expr1: TypeExpression, expr2: TypeExpression) => TypeFn(expr1, expr2) }
		),
		typeexpr2 -> (
			typeexpr3 ^^ (identity[TypeExpression] _) |
			typeexpr3 ~ STAR ~ tupletypeexpr ^^ { (head: TypeExpression, tail: List[TypeExpression]) => TypeTuple(head :: tail: _*) }
		),
		typeexpr3 -> (
			LBRACKET ~ typeexpr ~ RBRACKET ^^ (identity[TypeExpression] _) |
			SQIDENTIFIER ^^ { (str: String) => getTypeVar(str) }
		),
		tupletypeexpr -> (
			typeexpr3 ^^ { (expr: TypeExpression) => List(expr) } |
			typeexpr3 ~ STAR ~ tupletypeexpr ^^ { (head: TypeExpression, tail: List[TypeExpression]) => head :: tail }
		),
		paramlist -> (
			SQIDENTIFIER ^^ { (str: String) => List(getTypeVar(str)) } |
			SQIDENTIFIER ~ paramlist ^^ { (head: String, tail: List[TypeVariable]) => getTypeVar(head) :: tail }
		),
		cdecl -> (
			IDENTIFIER ^^ { (str: String) => (str, None) } |
			IDENTIFIER ~ OF ~ typeexpr ^^ { (str: String, expr: TypeExpression) => (str, Some(expr)) }
		),
		cdecllist -> (
			cdecl ^^ { (c: CDecl) => List(c) } |
			cdecl ~ PIPE ~ cdecllist ^^ { (head: CDecl, tail: List[CDecl]) => head :: tail }
		),
		typeheader -> (
			TYPE ~ IDENTIFIER ~ BIND ^^ { (str: String) => (str, List[TypeVariable]()) } |
			TYPE ~ SQIDENTIFIER ~ IDENTIFIER ~ BIND ^^ { (param: String, str: String) => (str, List(getTypeVar(param))) } |
			TYPE ~ LBRACKET ~ paramlist ~ RBRACKET ~ IDENTIFIER ~ BIND ^^ { (params: List[TypeVariable], str: String) => (str, params) }
		),
		typedef -> (
			typeheader ~ cdecllist ^^ { (header: (String, List[TypeVariable]), cdecls: List[CDecl]) => Data(header._1, header._2, cdecls: _*) }
		)

	)

	def chain(terminal: Symbol, action: (Expression, Expression) => Expression) =
		(expr ~ terminal ~ expr) ^^ { (expr1: Expression, expr2: Expression) => action(expr1, expr2) }

}