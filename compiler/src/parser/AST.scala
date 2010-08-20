
package parser.ast

import entry.Util._

package expressions {

	import patterns.Pattern

	object Expression {
		import PrettyPrinter._

		val prettyPrint: Expression => String = {
			case Id(name) => name
			case Const(value) => value.toString
			case IfThenElse(c, t, f) => "if (%) then (%) else (%)".pformat(c, t, f)
			case Let(p, d, b) => "let % = (%) in (%)".pformat(p, d, b)
			case LetRec(b, defs @ _*) =>
				defs map { d => "% = (%)".pformat(d._1, d._2) } mkString("let rec ", "and", " in " + b.pp)
			case UnOp(op, e) => "%(%)".pformat(op, e)
			case BinOp(op, e1, e2) => print(op.toString, e1, e2)
			case App(func, args @ _*) => "(%) %".pformat(func, print(" ", args: _*))
			case e: ListExpression => "[%]" pformat print(";", e: _*)
			case Tuple(exprs @ _*) => "(%)" pformat print(",", exprs: _*)
			case TupleElem(tuple, nr) => "% @ %".pformat(tuple.pp, nr)
			case Record(defs @ _*) => defs map { d => "% = %".pformat(d._1, d._2) } mkString ("{", ";", "}")
			case Field(rec, name) => "(%).%".pformat(rec, name)
			case Match(scrutinee, clauses @ _*) =>
				clauses map { c => "% -> (%)".pformat(c._1, c._2) } mkString("match (%) with " pformat (scrutinee), " | ", "")
			case Lambda(body, pats @ _*) => "fun % -> (%)".pformat(pats mkString " ", body)
			case expr => expr.toString
		}

		val fromSeq = entry.Util.fromSeq(Nil, Cons.apply _)

	}

	sealed trait Expression extends PrettyPrintable {
		override def prettyPrint = Expression prettyPrint this

		def subst(e:Expression,res:Expression):Expression = {
			if(this == e) res else this match {
				case Sequence(e1,e2) => Sequence(e1.subst(e,res),e2.subst(e,res))
				case IfThenElse(c,e1,e2) => IfThenElse(c.subst(e,res),e1.subst(e,res),e2.subst(e,res))
				case Let(p,e1,e2) => Let(p,e1.subst(e,res),e2.subst(e,res))
				case LetRec(e1,pe2@_*) => 
					LetRec(e1.subst(e,res),(pe2.toList map {case (p,e2) => (p,e2.subst(e,res))}):_*)
				case BinOp(op,e1,e2) => BinOp(op,e1.subst(e,res),e2.subst(e,res))
				case UnOp(op,e1) => UnOp(op,e1.subst(e,res))
				case App(f,args@_*) => App(f.subst(e,res),(args.toList map { _.subst(e,res) }):_*)
				case Cons(e1,e2) => Cons(e1.subst(e,res),e2.subst(e,res))
				case Tuple(ts@_*) => Tuple((ts.toList map { _.subst(e,res) }):_*)
				case TupleElem(t,x) => TupleElem(t.subst(e,res),x)
				case Record(fs@_*) => Record((fs.toList map { case (id,exp) => (id,exp.subst(e,res)) }):_*)
				case Field(r,x) => Field(r.subst(e,res),x)		
				case Match(e1,pe2@_*) => 
					Match(e1.subst(e,res),(pe2.toList map {case (p,e2) => (p,e2.subst(e,res))}):_*)
				case Lambda(body,args) => Lambda(body.subst(e,res),args)
				case x => x
			}
		}
	}
	
	final case class Id(name: String) extends Expression
	sealed trait Const[T] extends Expression { val value: T }
	final case class Integer(override val value: Int) extends Const[Int]
	final case class Bool(override val value: Boolean) extends Const[Boolean]
	final case class Character(override val value: Char) extends Const[Char]
	final case class Sequence(expr1: Expression, expr2: Expression) extends Expression
	final case class IfThenElse(cond: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression
	final case class Let(pattern: Pattern, definition: Expression, body: Expression) extends Expression
	final case class LetRec(body: Expression, patDef: (patterns.Id, Expression)*) extends Expression
	final case class BinOp(op: BinaryOperator.Value, expr1: Expression, expr2: Expression) extends Expression
	final case class UnOp(op: UnaryOperator.Value, expr: Expression) extends Expression
	final case class App(func: Expression, param: Expression*) extends Expression
	sealed trait ListExpression extends Expression with SList[Expression, ListExpression]
	final case class Cons(override val head: Expression, override val tail: Expression) extends ListExpression with SCons[Expression, ListExpression]
	case object Nil extends ListExpression with SNil[Expression, ListExpression]
	final case class Tuple(exprs: Expression*) extends Expression
	final case class TupleElem(tuple: Expression, nr: Int) extends Expression
	final case class Record(defs: (Id, Expression)*) extends Expression
	final case class Field(record: Expression, name: Id) extends Expression
	final case class Match(scrutinee: Expression, clauses: (Pattern, Expression)*) extends Expression
	final case class Lambda(body: Expression, arguments: Pattern*) extends Expression

	object Const {
		def unapply[T](c: Const[T]) = Some(c.value)
	}

	sealed trait Operator extends Enumeration {
		protected[this] def op(symbol: String) = Value(symbol)
	}
	
	object UnaryOperator extends Operator {
		val neg = op("-")
		val not = op("not")
	}
	
	object BinaryOperator extends Operator {
		val add = op("+")
		val sub = op("-")
		val mul = op("*")
		val div = op("/")
		val eq = op("==")
		val neq = op("<>")
		val geq = op(">=")
		val leq = op("<=")
		val gr = op(">")
		val le = op("<")
		val and = op("&")
		val or = op("or")
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

  abstract class TypeConstructor(val name: String, val params: TypeExpression*) extends TypeExpression {
//    override def toString = name match {
//      case "Function" => params(0) + " -> " + params(1)
//      case "List" => "[" + params(0) + "]"
//      case "Tupel" => "(" + params.mkString(",") + ")"
//      case n => n
//    }
  }

	object TypeConstructor {

		def unapplySeq(t: TypeConstructor): Option[(String, Seq[TypeExpression])] = Some((t.name, t.params))
		
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
  final case class Data(override val name: String, params: List[TypeVariable], declaration: (String, Option[TypeExpression])*) extends TypeDefinition
  final case class Record(override val name: String, fields: (String, TypeExpression)*) extends TypeDefinition
}

package patterns {

	object Pattern {
		import PrettyPrinter._

		private implicit def uncons(expr: Pattern): List[Pattern] = expr match {
			case Cons(head, tail) => head :: uncons(tail)
			case Nil => List()
			case rest => List(rest)
		}

		val prettyPrint: Pattern => String = {
			case Id(name) => name
			case Const(value) => value.toString
			case Underscore => "_"
			case Record(pats @ _*) => pats map { p => "% = %".pformat(p._1, p._2) } mkString ("{", ";", "}")
			case e: ListPattern => "[%s]" format print(";", e: _*)
			case Alternative(pat1, pat2) => "(%) | (%)".pformat(pat1, pat2)
			case Tuple(pats @ _*) => "(%s)" format print(",", pats: _*)
			case pat => pat.toString
		}

		val fromSeq = entry.Util.fromSeq(Nil, Cons.apply _)

	}

	sealed trait Pattern extends PrettyPrintable {
		override def prettyPrint = Pattern prettyPrint this
	}

	final case class Id(name: String) extends Pattern
	sealed trait Const[T] extends Pattern { val value: T }
	final case class Integer(override val value: Int) extends Const[Int]
	final case class Bool(override val value: Boolean) extends Const[Boolean]
	final case class Character(override val value: Char) extends Const[Char]
	case object Underscore extends Pattern
	final case class Record(patterns: (Id, Pattern)*) extends Pattern
	sealed trait ListPattern extends Pattern with SList[Pattern, ListPattern]
	case object Nil extends ListPattern with SNil[Pattern, ListPattern]
	final case class Cons(override val head: Pattern, override val tail: Pattern) extends ListPattern with SCons[Pattern, ListPattern]
	final case class Alternative(pat1: Pattern, pat2: Pattern) extends Pattern
	final case class Tuple(patterns: Pattern*) extends Pattern

	object Const {
		def unapply[T](c: Const[T]) = Some(c.value)
	}

}

