
package parser.ast

package expressions {

	import patterns.Pattern

	sealed trait Expression
	final case class Id(name: String) extends Expression
	sealed trait Const extends Expression
	final case class Integer(value: Int) extends Const
	final case class Bool(value: Boolean) extends Const
	final case class Character(value: Char) extends Const
	final case class Sequence(expr1: Expression, expr2: Expression) extends Expression
	final case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
	final case class Let(pattern: Pattern, definition: Expression, body: Expression) extends Expression
	final case class LetRec(body: Expression, patDef: (Pattern, Expression)*) extends Expression
	final case class BinOp(op: BinaryOperator.Value, expr1: Expression, expr2: Expression) extends Expression
	final case class App(func: Expression, param: Expression*) extends Expression
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
		val add, sub, mul, div, 
                  eq, neq, geq, leq, gr, le,
                  and, or = Value
	}
}

package types {

	sealed trait TypeExpression
	final case class TypeVariable(name: String) extends TypeExpression
	final case class TypeConstructor(name: String, params: TypeExpression*) extends TypeExpression

	sealed trait TypeDefinition { val name: String }
	final case class Data(override val name: String, params: Seq[TypeVariable], declaration: (String, Seq[TypeExpression])*) extends TypeDefinition
	final case class Record(override val name: String, fields: (String, TypeExpression)*) extends TypeDefinition

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

