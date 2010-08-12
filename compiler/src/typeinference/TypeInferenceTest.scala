/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package typeinference

object TypeInferenceTest {

  import parser.ast._
  import parser.ast.expressions._

  def main(args: Array[String]): Unit = {

    // unification problem derived from type checking palindrome
    //val u1 = (TFn(TypeVariable(2), TFn(TypeVariable(2), TBool())),
    //          TFn(TypeVariable(1), TFn(TypeVariable(4), TypeVariable(5))))

    val id = Lambda(Id("x"), patterns.Id("x"))
    val e2 = Let(patterns.Id("1"),
		 Lambda(Id("2"), patterns.Id("2")),
		 App(Id("1"),Id("1")))

    // e4: \f g h -> (f g) (g h) : ((a -> b) -> b -> c) -> (a -> b) -> a -> c
    val e4 = Lambda(Lambda(Lambda(
	  App(App(Id("1"),Id("2")), App(Id("2"),Id("3"))), patterns.Id("3")), patterns.Id("2")), patterns.Id("1"))

    // e5: \f g h -> (f g) (f h) unification fail
    // val e5 = Lambda(Var(1), Lambda((Var(2)), Lambda(Var(3),
    //    (App(App(V(Var(1)), V(Var(2))), App(V(Var(1)), V(Var(3))))))))

    // e6: let f = \x -> x in let y = f f in y f: a->a
    val e6 = Let(patterns.Id("1"),
		 Lambda(Id("2"), patterns.Id("2")),
		 Let(patterns.Id("2"),
		     App(Id("1"), Id("1")),
		     App(Id("2"),Id("1"))))
    val e7 = Lambda(Id("xs"), patterns.Cons(patterns.Id("x"),patterns.Id("xs")))
    val xy = Lambda(Lambda(Id("xs"), patterns.Cons(patterns.Id("x"), patterns.Id("xs"))),
                    patterns.Cons(patterns.Id("y"), patterns.Id("ys")))
    val e9 = Lambda(Id("x"), patterns.Alternative(patterns.Cons(patterns.Id("x"),patterns.Id("xs")),
						  patterns.Cons(patterns.Id("x"),patterns.Nil)))
    val e11 = Let(patterns.Id("x"), Record((Id("one"), Integer(2))), Field(Id("x"), Id("one")))
    val e12 = Let(patterns.Id("x"),
		  Record((Id("one"), Integer(2)), (Id("inc"), Lambda(Id("x"), patterns.Id("x")))),
		  App(Field(Id("x"), Id("inc")), Integer(2)))
    val e13 = Let(patterns.Id("x"),
		  Record((Id("one"), Integer(2)),
			 (Id("rec2"), Record((Id("inc"), Lambda(Id("x"), patterns.Id("x")))))),
		  App(Field(Field(Id("x"), Id("rec2")), Id("inc")), Integer(2)))

    // nested record statement
    val e14 = Let(patterns.Id("x"),
		  Record((Id("one"), Integer(2)),
			 (Id("rec2"), Record((Id("inc"), Lambda(Id("x"), patterns.Id("x")))))),
		  Field(Field(Id("x"), Id("rec2")), Id("inc")))
    
    // TODO: how to specify tuple? -> PatSeq?
    //   val e10 = Lambda(Id("x"), patterns.Tuple(patterns.Id("x"), Id("y")))

    // function with multiple arguments
    val e15 = Lambda(Id("xs"), patterns.Cons(patterns.Id("x"), patterns.Id("xs")),
		     patterns.Cons(patterns.Id("y"), patterns.Id("ys")))

    val e16 = Let(patterns.Cons(patterns.Id("x"), patterns.Id("xs")),
		  Cons(Integer(1), Cons(Integer(2), Nil)),
		  Id("xs"))

    val e17 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("y")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("y"))), patterns.Id("x"))

    // should fail (expressions aren't of same type)
    val e18 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("y")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("x"))), patterns.Id("x"))

    // 1 -> 1
    val e19 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("ys")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("x"))), patterns.Id("x"))

    // 2 -> 2
    val e20 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("ys")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Nil)), patterns.Id("x"))

    // 1 -> 2
    val e21 = Lambda(Integer(1), patterns.Underscore)

    // [1] -> 2
    val e22 = Lambda(Integer(1), patterns.Nil)

    // Integer
    val e23 = Let(patterns.Cons(patterns.Underscore, patterns.Id("x")),
		  Cons(Integer(1), Cons(Integer(2), Nil)),
		  Id("x"))

    // should succeed, type: Integer
    val e24 = Let(patterns.Nil, Cons(Integer(1), Nil), Integer(1))

    // should fail, texpr is not a list
    val e25  = Let(patterns.Nil, Integer(1), Integer(1))

    // pattern match: should succeed: type Integer
    val e26 = Let(patterns.Record((patterns.Id("one"), patterns.Cons(patterns.Id("x"), patterns.Nil))),
                  Record((Id("one"), Cons(Integer(1),Nil))),
                  Integer(2))

    // pattern match should fail
    val e27 = Let(patterns.Record((patterns.Id("one"), patterns.Cons(patterns.Id("x"), patterns.Nil))),
                  Integer(3),
                  Integer(2))

    val e28 = IfThenElse(Bool(true), Integer(1), Integer(2))

    // test should fail
    val e29 = IfThenElse(Bool(true), Integer(1), Bool(false))

    val e30 = UnOp(UnaryOperator.neg, Integer(1))

    // test should fail
    val e31 = UnOp(UnaryOperator.neg, Bool(true))

    val e32 = UnOp(UnaryOperator.not, Bool(true))

    // test should fail
    val e33 = UnOp(UnaryOperator.not, Integer(1))

    val e40 = Let(patterns.Tuple(patterns.Id("x"),patterns.Integer(1)),
		  Tuple(Integer(1),Integer(2)),
		  Id("x"))
    
    // println("e2: " + typeCheck(List(), e2))
    // println("e4: " + typeCheck(List(), e4))
    // println("e5: " + type¸Check(List(), e5))
    // println("e7: " + typeCheck(List(), e9))
    // println(unify(List((TList(TInt()),TList(TypeVariable(2))))))
//    println("e16: " + TypeInference.typeCheck(List(), e16))
//    println("e18: " + TypeInference.typeCheck(List(), e18))
//    println("e19: " + TypeInference.typeCheck(List(), e19))
//    println("e20: " + TypeInference.typeCheck(List(), e20))
//    println("e21: " + TypeInference.typeCheck(List(), e21))
//    println("e22: " + TypeInference.typeCheck(List(), e22))
//    println("e23: " + TypeInference.typeCheck(List(), e23))
//    println("e24: " + TypeInference.typeCheck(List(), e24))
//    println("e25: " + TypeInference.typeCheck(List(), e25))
    println("e26: " + TypeInference.typeCheck(List(), e26))
    println("e27: " + TypeInference.typeCheck(List(), e27))
//    println("e28: " + TypeInference.typeCheck(List(), e28))
//    println("e28: " + TypeInference.typeCheck(List(), e28))
//
//    try {
//      println("e29: " + TypeInference.typeCheck(List(), e29))
//      println("This shouldn't happen e29")
//    } catch {
//      case _:TypeInference.TypeError => println("Expected Failure for e29")
//    }
//
//    println("e30: " + TypeInference.typeCheck(List(), e30))
//
//    try {
//      println("e31: " + TypeInference.typeCheck(List(), e31))
//      println("This shouldn't happen e31")
//    } catch {
//      case _:TypeInference.TypeError => println("Expected Failure for e31")
//    }
//
//    println("e32: " + TypeInference.typeCheck(List(), e32))
//
//    try {
//      println("e33: " + TypeInference.typeCheck(List(), e33))
//      println("This shouldn't happen e33")
//    } catch {
//      case _:TypeInference.TypeError => println("Expected Failure for e33")
//    }
//
//    println("e40: " + TypeInference.typeCheck(List(), e40))
  }
}
