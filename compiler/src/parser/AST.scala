
package parser.ast

package expressions {

	import patterns.Pattern

	object Expression {
		private def print(sep: String, exprs: Expression*) = exprs map prettyPrint mkString("(", ")"+sep+"(", ")")

		private val uncons: Expression => List[Expression] = {
			case Cons(head, tail) => head :: uncons(tail)
			case Nil => List()
			case rest => List(rest)
		}

		val prettyPrint: Expression => String = {
			case Id(name) => name
			case Const(value) => value.toString
			case IfThenElse(c, t, f) => "if (%s) then (%s) else (%s)".format(prettyPrint(c), prettyPrint(t), prettyPrint(f))
			case Let(p, d, b) => "let %s = (%d) in (%b)".format(p, prettyPrint(d), prettyPrint(b))
			case LetRec(b, defs @ _*) =>
				defs map { d => "%s = (%s)".format(d._1.name, prettyPrint(d._2)) } mkString("let rec ", "and", " in " + prettyPrint(b))
			case UnOp(op, e) => "%s(%s)".format(op, prettyPrint(e))
			case BinOp(op, e1, e2) => print(op.toString, e1, e2)
			case App(func, args @ _*) => "(%s) %s".format(prettyPrint(func), print(" ", args: _*))
			case e: Cons => "[%s]" format print(";", uncons(e): _*)
			case Tuple(exprs @ _*) => "(%s)" format print(",", exprs: _*)
			case TupleElem(tuple, nr) => "%s @ %d".format(prettyPrint(tuple), nr)
			case Record(defs @ _*) => defs map { d => "%s: %s".format(d._1, prettyPrint(d._2)) } mkString ("{", ";", "}")
			case Field(rec, name) => "(%s).%s".format(prettyPrint(rec), name)
			case Match(scrutinee, clauses @ _*) =>
				clauses map { c => "%s -> (%s)".format(c._1, prettyPrint(c._2)) } mkString("match (%s) with " format (prettyPrint(scrutinee)), " | ", "")
			case Lambda(body, pats @ _*) => "fun %s -> (%s)".format(pats mkString " ", prettyPrint(body))
			case expr => expr.toString
		}

        def fromSeq(list: List[Expression]): ListExpression = list match {
			case List() => Nil
			case head :: tail => Cons(head, fromSeq(tail))
		}

	}

	sealed trait Expression {
		def prettyPrint = Expression prettyPrint this

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
	sealed trait ListExpression extends Expression
	final case class Cons(head: Expression, tail: Expression) extends ListExpression
	case object Nil extends ListExpression
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
		def fromSeq(list: List[Pattern]): ListPattern = list match {
			case List() => Nil
			case head :: tail => Cons(head, fromSeq(tail))
		}
	}

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

