
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

}
