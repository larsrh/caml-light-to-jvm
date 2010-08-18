
package typeinference.test

import parser.ast._
import parser.ast.types._
import parser.ast.expressions._
import typeinference._
import typeinference.TypeInference.TypeError
import org.junit._
import Assert._

class TypeInferenceTest {


  @Test
  def test_Let_Match_Literal = {
    // should type check; would throw an pattern match
    val e = Let(patterns.Integer(1), Integer(2), Integer(3))
    assertEquals((List(), TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e))
  }


  @Test
  def test_Lambda_Match_Literal = {
    // should type check; would throw an pattern match
    val e = Lambda(Integer(2), patterns.Integer(1))
    assertEquals((List(), TypeFn(TypeInt(), TypeInt())),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e))
  }

  def test_Oddeven = {
    val test = LetRec(BinOp(BinaryOperator.and, App(Id("even"), Integer(4)), UnOp(UnaryOperator.not, App(Id("odd"), Integer(0)))),
		      (patterns.Id("odd"), Lambda(Match(Id("x"),
							(patterns.Integer(0), Bool(false)),
							(patterns.Underscore, App(Id("even"), BinOp(BinaryOperator.sub, Id("x"), Integer(1))))),
						  patterns.Id("x"))),
		      (patterns.Id("even"), Lambda(Match(Id("y"),
							 (patterns.Integer(0), Bool(true)),
							 (patterns.Underscore, App(Id("odd"), BinOp(BinaryOperator.sub, Id("y"), Integer(1))))),
						   patterns.Id("y"))))

    assertEquals((List(), TypeBool()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test
  def test_Complexmatch = {
    val test = LetRec(Match(App(Id("list"), Integer(5)),
			    (patterns.Nil, Integer(42)),
			    (patterns.Cons(patterns.Id("n1"), patterns.Cons(patterns.Id("n2"), patterns.Cons(patterns.Id("n3"), patterns.Nil))), Id("n2")),
			    (patterns.Cons(patterns.Id("n1"), patterns.Id("n2")), Match(Id("n2"),
											(patterns.Nil, Integer(42)),
											(patterns.Cons(patterns.Id("n1"), patterns.Id("n2")), Id("n1"))))
      ),
		      (patterns.Id("list"), Lambda(Match(Id("n"),
							 (patterns.Integer(0), Nil),
							 (patterns.Underscore, Cons(Id("n"), App(Id("list"), BinOp(BinaryOperator.sub, Id("n"), Integer(1)))))),
						   patterns.Id("n"))))

    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test
  def test_Record0 = {
    // let {x = 0} = {x = 1} in 1
    val test = Let(patterns.Record((patterns.Id("x"), patterns.Integer(0))),
		   expressions.Record((Id("x"), Integer(1))),
		   Integer(1))

    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test (expected=classOf[TypeError])
  def test_Record1 {
    // let {x = 0} = {x = true} in 1
    val test = Let(patterns.Record((patterns.Id("x"), patterns.Bool(true))),
		   expressions.Record((Id("x"), Integer(1))),
		   Integer(1))

    TypeInference.typeCheck(TypeInference.emptyEnv, test)
  }

  @Test
  def test_Record2 = {
    val test = Let(patterns.Id("x"), expressions.Record((Id("y"), Lambda(Integer(1), patterns.Integer(2)))), Integer(0))

    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test
  def test_Tuple0 = {
    val test = Let(patterns.Id("x"), expressions.Tuple(Integer(1), Integer(2), Integer(3)),
		   Id("x"))

    assertEquals((List(), TypeTuple(TypeInt(), TypeInt(), TypeInt())),
		 TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

//  @Test
//  def test_Tuple1 = {
//    // let foo = fun u -> (1, true, 3) in let (x, y, z) = foo 0 in y;;
//    val test = Let(patterns.Id("foo"), Lambda(Tuple(Integer(1), Bool(true), Integer(3)), patterns.Id("u")),
//				      Let(patterns.Tuple(patterns.Id("x"), patterns.Id("y"), patterns.Id("z")), App(Id("foo"), Integer(0)), Id("y")))
//
//    assertEquals((List(), TypeBool()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
//  }

  @Test
  def test_parseInt = {
    val parseminus = Lambda(IfThenElse(BinOp(BinaryOperator.eq,Id("x"),Character('-')),
				       Tuple(Lambda(UnOp(UnaryOperator.neg,Id("z")),patterns.Id("z")), Id("xs")),
				       Tuple(Lambda(Id("z"),patterns.Id("z")), Cons(Id("x"), Id("xs")))),
			    patterns.Cons(patterns.Id("x"),patterns.Id("xs")))
    val parsepositive = Lambda(Match(Id("list"), (patterns.Nil, Id("acc")),
				     (patterns.Cons(patterns.Id("x"),patterns.Id("xs")),
				      App(Id("parsepositive"),Id("xs"),
					  BinOp(BinaryOperator.add,
						BinOp(BinaryOperator.mul,Integer(10), Id("acc")),
						(App(Id("intofchar"), Id("x"))))))),
				     patterns.Id("list"),patterns.Id("acc"))

    val intofchar = (patterns.Id("intofchar"), Lambda(Match(Id("x"),
						(patterns.Character('1'), Integer(1)),
						(patterns.Character('2'), Integer(2)),
						(patterns.Character('3'), Integer(3)),
						(patterns.Character('4'), Integer(4)),
						(patterns.Character('5'), Integer(5)),
						(patterns.Character('6'), Integer(6)),
						(patterns.Character('7'), Integer(7)),
						(patterns.Character('8'), Integer(8)),
						(patterns.Character('9'), Integer(9)),
						(patterns.Character('0'), Integer(0))
    ), patterns.Id("x")))

    val body = Let(patterns.Id("parsepos"), Lambda(App(Id("sign"), App(Id("parsepositive"), Id("list"), Integer(0))), patterns.Tuple(patterns.Id("sign"), patterns.Id("list"))),
		    Let(patterns.Id("o"),
//			Lambda(App(Id("f"), App(Id("g"), Id("x"))), patterns.Id("f"), patterns.Id("g"), patterns.Id("x")),
			Lambda(Lambda(Lambda(App(Id("f"), App(Id("g"), Id("x"))), patterns.Id("x")), patterns.Id("g")), patterns.Id("f")),
			BinOp(BinaryOperator.add,
			      App(App(Id("o"), Id("parsepos"), Id("parseminus")), Cons(Character('-'), Cons(Character('4'), Cons(Character('2'), Nil)))),
			      App(App(Id("o"), Id("parsepos"), Id("parseminus")), Cons(Character('4'), Cons(Character('2'), Nil))))))


    val test = LetRec(body, (patterns.Id("parseminus"), parseminus),
			    (patterns.Id("parsepositive"), parsepositive),
			    intofchar)

    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

//      LetRec(Let(Id("parsepos""),
//	       Lambda(Match(Id("06"),List((Tuple(List(Id("sign"), Id("list"))),
//	       App(Id("sign),List(App(Id("parsepositive"),List(Id("list"), Integer(0)))))))),
//	       List(Id("06"))),Let(Id("o"),Lambda(Match(Id("09"),
//	       List((Id("f"),Lambda(Match(Id("08"),
//	       List((Id("g"),Lambda(Match(Id("07"),List((Id("x"),App(Id("f"),
//	       List(App(Id("g"),List(Id("x")))))))),List(Id("07")))))),
//	       List(Id("08")))))),List(Id("09"))),BinOp(add,App(Id("o"),List(Id("parsepos"), Id("parseminus"),
//	       Cons(Character('-'),Cons(Character('4'),Cons(Character('2'),Nil))))),App(Id("o"),
//	       List(Id("parsepos"), Id("parseminus"), Cons(Character('4'),Cons(Character('2'),Nil))))))),
//	       List((Id("parseminus"),Lambda(Match(Id("02"),List((Cons(Id("x"),Id("xs")),
//	       IfThenElse(BinOp(eq,Id("x"),Character('-')),Tuple(List(Lambda(Match(Id("00"),
//	       List((Id("z"),UnOp(neg,Id("z"))))),List(Id("00"))), Id("xs"))),
//	       Tuple(List(Lambda(Match(Id("01"),List((Id("z"),Id("z")))),List(Id("01"))),
//	       Cons(Id("x"),Id("xs")))))))),List(Id("02")))), (Id("parsepositive"),Lambda(Match(Id("04"),
//	       List((Id("list"),Lambda(Match(Id("03"),List((Id("acc"),Match(Id("list"),List((Nil,Id("acc")),
//	       (Cons(Id("x"),Id("xs")),App(Id("parsepositive"),List(Id("xs"), BinOp(add,BinOp(mul,Integer(10),
//	       Id("acc")),App(Id("intofchar"),List(Id("x")))))))))))),List(Id("03")))))),List(Id("04")))),
//	       (Id("intofchar"),Lambda(Match(Id("05"),List((Id("x"),Match(Id("x"),List((Character('1'),Integer(1)),
//	       (Character('2'),Integer(2)), (Character('3'),Integer(3)), (Character('4'),Integer(4)),
//	       (Character('5'),Integer(5)), (Character('6'),Integer(6)), (Character('7'),Integer(7)),
//	       (Character('8'),Integer(8)), (Character('9'),Integer(9)), (Character('0'),Integer(0))))))),List(Id("05"))))))

}

