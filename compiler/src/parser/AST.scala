
package parser.ast

package expressions {

	import patterns.Pattern

	sealed trait Expression
	final case class Id(name: String) extends Expression
	final case class Const(value: Int) extends Expression
	final case class Seq(expr1: Expression, expr2: Expression) extends Expression
	final case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
	final case class Let(pattern: Pattern, definition: Expression, body: Expression) extends Expression
	final case class LetRec(pattern: Pattern, definition: Expression, body: Expression) extends Expression
	final case class BinOp(op: BinaryOperator.Value, expr1: Expression, expr2: Expression) extends Expression
	final case class App(func: Expression, param: Expression) extends Expression
	sealed trait ListExpression extends Expression
	final case class Cons(head: Expression, tail: Expression) extends ListExpression
	case object Nil extends ListExpression
	final case class Tuple(exprs: Expression*) extends Expression
	final case class Record(defs: (Id, Expression)*) extends Expression
	final case class Field(record: Expression, name: Id) extends Expression
	final case class Match(scrutinee: Expression, clauses: (Pattern, Expression)*) extends Expression
	final case class Lambda(body: Pattern, arguments: Pattern*) extends Expression

	object BinaryOperator extends Enumeration {
		type BinaryOperator = Value
		// TODO fill in operators
	}

}

package types {

	import scala.collection.immutable.List

	sealed trait TypeExpression
	final case class TypeVariable(name: String) extends TypeExpression
	final case class TypeConstructor(name: String, params: TypeExpression*) extends TypeExpression

	sealed trait TypeDefinition { val name: String }
	final case class Data(name: String, params: List[TypeVariable], declaration: (String, List[TypeExpression])*) extends TypeDefinition
	final case class Record(name: String, fields: (String, TypeExpression)*) extends TypeDefinition

}

package patterns {

	sealed trait Pattern
	final case class Id(name: String) extends Pattern
	case object Underscore extends Pattern
	final case class Record(patterns: (Id, Pattern)*) extends Pattern
	sealed trait ListPattern extends Pattern
	case object Nil extends ListPattern
	final case class Cons(head: Pattern, tail: Pattern) extends ListPattern
	final case class Alternative(pat1: Pattern, pat2: Pattern) extends Pattern
	final case class Tuple(patterns: Pattern) extends Pattern

}

