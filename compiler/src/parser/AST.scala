
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
  final case class LetRec(body: Expression, patDef: (patterns.Id, Expression)*) extends Expression
  final case class BinOp(op: BinaryOperator.Value, expr1: Expression, expr2: Expression) extends Expression
  final case class UnOp(op: UnaryOperator.Value, expr: Expression) extends Expression
  final case class App(func: Expression, param: Expression*) extends Expression
  sealed trait ListExpression extends Expression
  final case class Cons(head: Expression, tail: Expression) extends ListExpression
  case object Nil extends ListExpression
  final case class Tuple(exprs: Expression*) extends Expression
	final case class TupleElem(tuple: Expression, nr: Int) extends Expression
  final case class Record(defs: (Id, Expression)*) extends Expression
  final case class Field(record: Expression, name: Id) extends Expression
  final case class Match(scrutinee: Expression, clauses: (Pattern, Expression)*) extends Expression
  final case class Lambda(body: Expression, arguments: Pattern*) extends Expression

  sealed trait Operator extends Enumeration
	
  object UnaryOperator extends Operator {
    type UnaryOperator = Value
    val neg, not = Value
  }
	
  object BinaryOperator extends Operator {
    type BinaryOperator = Value
    val add, sub, mul, div,
    eq, neq, geq, leq, gr, le,
    and, or = Value
  }
}

package types {

  trait Subst[A] {
    def subst_(s: (TypeExpression,TypeExpression)): A
  }

  sealed abstract trait TypeExpression extends Subst[TypeExpression] {
    def subst(t: List[(TypeExpression,TypeExpression)]): TypeExpression = 
      t.foldLeft(this){(a: TypeExpression, t: (TypeExpression,TypeExpression)) => a.subst_(t)}
  }

  final case class TypeVariable(i: Int) extends TypeExpression {
    def subst_(s: (TypeExpression,TypeExpression)) =
      if (this == s._2) { s._1 } else { this }
  }

  abstract case class TypeConstructor(name: String, params: TypeExpression*) extends TypeExpression {
//    override def toString = name match {
//      case "Function" => params(0) + " -> " + params(1)
//      case "List" => "[" + params(0) + "]"
//      case "Tupel" => "(" + params.mkString(",") + ")"
//      case n => n
//    }
  }

  case class TypeInt() extends TypeConstructor("Integer") {
    def subst_(t: (TypeExpression,TypeExpression)) = TypeInt()
  }

  case class TypeBool() extends TypeConstructor("Boolean") {
    def subst_(t: (TypeExpression,TypeExpression)) = TypeBool()
  }

  case class TypeChar() extends TypeConstructor("Character") {
    def subst_(t: (TypeExpression,TypeExpression)) = TypeChar()
  }

  case class TypeFn(from: TypeExpression, to: TypeExpression) extends TypeConstructor("Function", from, to) {
    def subst_(s: (TypeExpression,TypeExpression)) =
      TypeFn(from.subst_(s), to.subst_(s))
  }

  case class TypeList(t: TypeExpression) extends TypeConstructor("List", t) {
    def subst_(s: (TypeExpression, TypeExpression)) = TypeList(t.subst_(s))
  }

  case class TypeTuple(types: TypeExpression*) extends TypeConstructor("Tupel", types:_*) {
    def subst_(s: (TypeExpression, TypeExpression)) =
      TypeTuple(types.map(x => x.subst_(s)):_*)
  }

  case class TypeRecord(override val name: String, fields: (String, TypeExpression)*) extends TypeConstructor("Record", fields map (f => f._2):_*) {
    def subst_(s: (TypeExpression, TypeExpression)) = {
      TypeRecord(name, fields.map(x => (x._1, x._2.subst_(s))):_*)
    }
  }

  sealed trait TypeDefinition { val name: String }
  final case class Data(override val name: String, params: Seq[TypeVariable], declaration: (String, Seq[TypeExpression])*) extends TypeDefinition
  final case class Record(override val name: String, fields: (String, TypeExpression)*) extends TypeDefinition

}

package patterns {

  sealed trait Pattern
  final case class Id(name: String) extends Pattern
  sealed trait Const extends Pattern
  final case class Integer(value: Int) extends Const
  final case class Bool(value: Boolean) extends Const
  final case class Character(value: Char) extends Const
  case object Underscore extends Pattern
  final case class Record(patterns: (Id, Pattern)*) extends Pattern
  sealed trait ListPattern extends Pattern
  case object Nil extends ListPattern
  final case class Cons(head: Pattern, tail: Pattern) extends ListPattern
  final case class Alternative(pat1: Pattern, pat2: Pattern) extends Pattern
  final case class Tuple(patterns: Pattern*) extends Pattern

}

