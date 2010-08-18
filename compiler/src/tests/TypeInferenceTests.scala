
package tests

import parser.ast._
import parser.ast.types._
import parser.ast.expressions._
import typeinference._
import typeinference.TypeInference.TypeError

class TypeInferenceTests extends TestSuite {

	test("U1", {
		val u1 = (TypeFn(TypeVariable(2), TypeFn(TypeVariable(2), TypeBool())),
				  TypeFn(TypeVariable(1), TypeFn(TypeVariable(4), TypeVariable(5))))
		assertEquals(List((TypeVariable(1),TypeVariable(2)),
						  (TypeVariable(4),TypeVariable(1)),
						  (TypeBool(),TypeVariable(5))),
					 TypeInference.unify(List(u1)))
	})

	test("idFn", {
		val e1 = Lambda(Id("x"), patterns.Id("x"))
		assertEquals((List(TypeVariable(1)), TypeFn(TypeVariable(1),TypeVariable(1))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e1))
	})

	test("Let_Pattern_Id", {
		// let x = \y -> y in x x
		val e2 = Let(patterns.Id("x"), Lambda(Id("y"), patterns.Id("y")), App(Id("x"),Id("x")))
		assertEquals((List(TypeVariable(4)),TypeFn(TypeVariable(4),TypeVariable(4))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e2))
	})

	test("Lambda_App_Curried", {
		val e3 = Lambda(Lambda(Lambda(
					App(App(Id("1"),Id("2")), App(Id("2"),Id("3"))), patterns.Id("3")), patterns.Id("2")), patterns.Id("1"))
		assertEquals((List(TypeVariable(3), TypeVariable(6), TypeVariable(4)),
					  TypeFn(TypeFn(TypeFn(TypeVariable(3),TypeVariable(6)),TypeFn(TypeVariable(6),TypeVariable(4))),
							 TypeFn(TypeFn(TypeVariable(3),TypeVariable(6)),TypeFn(TypeVariable(3),TypeVariable(4))))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e3))
	})

	test("Unification_Fail", {
		val e4 = Lambda(App(App(Id("1"), Id("2")), App(Id("1"), Id("3"))),
						patterns.Id("1"), patterns.Id("2"), patterns.Id("3"))

		calculate(TypeInference.typeCheck(Map(), e4)).shouldThrow[TypeError]
	})

	test("If_Curried_Equals_NonCurried", {
		val e3 = Lambda(Lambda(Lambda(
					App(App(Id("1"),Id("2")), App(Id("2"),Id("3"))), patterns.Id("3")), patterns.Id("2")), patterns.Id("1"))
		val e4m = Lambda(App(App(Id("1"), Id("2")), App(Id("2"), Id("3"))),
						 patterns.Id("1"), patterns.Id("2"), patterns.Id("3"))
		assertEquals(TypeInference.typeCheck(TypeInference.emptyEnv, e3),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e4m))
	})

	test("Nested_Let_With_Poly_Fun", {
		val e5 = Let(patterns.Id("1"), Lambda(Id("2"), patterns.Id("2")),
					 Let(patterns.Id("2"), App(Id("1"), Id("1")), App(Id("2"),Id("1"))))
		assertEquals((List(TypeVariable(7)),TypeFn(TypeVariable(7),TypeVariable(7))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e5))
	})

	test("Lambda_Cons_PatternMatch_Tail", {
		val e6 = Lambda(Id("xs"), patterns.Cons(patterns.Id("x"),patterns.Id("xs")))
		// should equal: [1] -> [1]
		assertEquals((List(TypeVariable(1)),TypeFn(TypeList(TypeVariable(1)),TypeList(TypeVariable(1)))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e6))
	})

	test("Lambda_Cons_PatternMatch_Head", {
		val e7 = Lambda(Id("x"), patterns.Cons(patterns.Id("x"),patterns.Id("xs")))
		// should equal: [1] -> 1
		assertEquals((List(TypeVariable(1)),TypeFn(TypeList(TypeVariable(1)),TypeVariable(1))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e7))
	})

	test("Lambda_Cons_Multiple_Patterns", {
		// \ (y:ys) -> \(x:xs) -> xs :: [a] -> [b] -> [b]
		val e8 = Lambda(Lambda(Id("xs"), patterns.Cons(patterns.Id("x"), patterns.Id("xs"))),
						patterns.Cons(patterns.Id("y"), patterns.Id("ys")))
		// [1] -> [3] -> [3]
		assertEquals((List(TypeVariable(1), TypeVariable(2)),
					  TypeFn(TypeList(TypeVariable(1)), TypeFn(TypeList(TypeVariable(2)),TypeList(TypeVariable(2))))),
					 TypeInference.typeCheck(TypeInference.emptyEnv, e8))
	})

	test("Basic_Record_Patttern_Match", {
		// let x = { one = 2 } in x.one :: Int
		val e9 = Let(patterns.Id("x"), expressions.Record((Id("one"), Integer(2))), Field(Id("x"), Id("one")))
		assertEquals((List(),TypeInt()),TypeInference.typeCheck(TypeInference.emptyEnv, e9))
	})

	test("Record_Pattern_Match_Multiple_Fields", {
		// let x = { one = 2, inc = \x -> x } in x.inc 2 :: Int
		val e10 = Let(patterns.Id("x"),
					  expressions.Record((Id("one"), Integer(2)), (Id("inc"), Lambda(Id("x"), patterns.Id("x")))),
					  App(Field(Id("x"), Id("inc")), Integer(2)))
		assertEquals((List(),TypeInt()),TypeInference.typeCheck(TypeInference.emptyEnv, e10))
	})

	test("NestedRecord_Pattern_Match", {
		// let x = { one = 2, rec2 = { inc = \x -> \x } } in
		val e11 = Let(patterns.Id("x"),
					  expressions.Record((Id("one"), Integer(2)),
										 (Id("rec2"), expressions.Record((Id("inc"), Lambda(Id("x"), patterns.Id("x")))))),
					  App(Field(Field(Id("x"), Id("rec2")), Id("inc")), Field(Id("x"), Id("one"))))
		assertEquals((List(),TypeInt()),TypeInference.typeCheck(TypeInference.emptyEnv, e11))
	})

	test("Lambda_Pattern_Match_Int", {
		val e12 = Let(patterns.Cons(patterns.Id("x"), patterns.Id("xs")),
					  Cons(Integer(1), Cons(Integer(2), Nil)), Id("xs"))
		assertEquals((List(),TypeList(TypeInt())), TypeInference.typeCheck(TypeInference.emptyEnv, e12))
	})

	test("Match_List_Value", {
		val e13 = Lambda(Match(Id("x"),
							   (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("y")),
							   (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("y"))), patterns.Id("x"))
		// TODO check
		println(TypeList(TypeVariable(2)), TypeInference.typeCheck(TypeInference.emptyEnv, e13))
		alwaysFail(())
	})

}
