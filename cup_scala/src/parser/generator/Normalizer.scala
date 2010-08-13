
package parser.generator

import parser.ast.expressions._

object Normalizer {

	def normalize(expr: Expression): Expression = expr match {
		case Sequence(e1, e2) => Sequence(normalize(e1), normalize(e2))
		case IfThenElse(c, t, f) => IfThenElse(normalize(c), normalize(t), normalize(f))
		case Let(p, d, b) => Let(p, normalize(d), normalize(b))
		case LetRec(b, p @ _*) => LetRec(normalize(b), p.toList: _*)
		case BinOp(BinaryOperator.sub, Integer(0), e) => UnOp(UnaryOperator.neg, normalize(e))
		case BinOp(op, e1, e2) => BinOp(op, normalize(e1), normalize(e2))
		case App(App(func, head @ _*), tail @ _*) => normalize(App(func, (head ++ tail).toList: _*))
		case App(func, args @ _*) => App(normalize(func), args map normalize toList: _*)
		case Cons(head, tail) => Cons(normalize(head), normalize(tail))
		case Tuple(exprs @ _*) => Tuple(exprs map normalize toList: _*)
		case TupleElem(tuple, nr) => TupleElem(normalize(tuple), nr)
		case Record(defs @ _*) => Record(defs map { d => (d._1, normalize(d._2)) } toList: _*)
		case Field(rec, name) => Field(normalize(rec), name)
		case Match(scrutinee, clauses @ _*) => Match(normalize(scrutinee), clauses map { c => (c._1, normalize(c._2)) } toList: _*)
		case Lambda(body, pats @ _*) => Lambda(normalize(body), pats.toList: _*)
		case _ => expr
	}


}
