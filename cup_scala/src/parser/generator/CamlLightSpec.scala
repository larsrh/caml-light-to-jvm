
package parser.generator

import edu.tum.cup2.grammar.{NonTerminal, Terminal, Symbol}
import edu.tum.cup2.semantics.{SymbolValue}
import edu.tum.cup2.spec.CUP2Specification
import edu.tum.cup2.spec.scala.{ScalaCUPSpecification, SymbolEnum}

class CamlLightSpec extends CUP2Specification with ScalaCUPSpecification {

	object Terminals extends SymbolEnum {
		val	IDENTIFIER,
			INTCONST,
			LCURLBRACE, RCURLBRACE, LSQBRACK, RSQBRACKET, LBRACE, RBRACE, // ( ) [ ] { }
			MUL, PLUS, MINUS, DIV, CONS, SEMI, POINT, // * + - / :: ; .
			LESS, LEQ, GREATER, GEQ, EQ, NEQ, // < <= > >= == <>
			FUN, FUNCTION, MATCH, PIPE, // fun function match |
			AND, OR, NOT, // &, or, not
			TYPE, // type
			IF, THEN, ELSE, // if then else
			IN, OF, LET, REC // in of let rec
			= TerminalEnum
	}

	object NonTerminals extends SymbolEnum {
		val expr, const, typeexpr, typedef, pattern = NonTerminalEnum
	}

	// make the enum-Values available
	import NonTerminals._, Terminals._

	// tell parent class what (non)terminals exist
	val terminals = Terminals
	val nonTerminals = NonTerminals

	import parser.ast.expressions._

	//symbols with values
	class INTCONST extends SymbolValue[Int]
	class IDENTIFIER extends SymbolValue[String]
	class expr extends SymbolValue[Expression]

	grammar(
		expr -> (
			IDENTIFIER ^^ { (str: String) => Id(str) } |
			INTCONST ^^ { (n: Int) => Integer(n) } |
			expr ~ SEMI ~ expr ^^ { (expr1: Expression, expr2: Expression) => Sequence(expr1, expr2) } |
			IF ~ expr ~ THEN ~ expr ~ ELSE ~ expr ^^ { (cond: Expression, ifTrue: Expression, ifFalse: Expression) => IfThenElse(cond, ifTrue, ifFalse) }
		)
	)


	//precedences
	/*precedences(left(TIMES), left(PLUS))

	// specify grammar
	grammar(
		res   ->  (  expr ~ SEMI ^^ { (x : Int) => x }   ),
		expr  ->  (  NUMBER ^^ { (x : Int) => x } |
				   expr ~ PLUS ~ expr ^^ { (x : Int, y : Int) => x + y} |
				   expr ~ TIMES ~ expr ^^ { (x : Int, y : Int) => x * y} |
				   LPAREN ~ expr ~ RPAREN ^^ { (x : Int) => x }
		)
	)*/
}
