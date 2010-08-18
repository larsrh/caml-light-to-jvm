
package typeinference.test

import parser.ast._
import parser.ast.types._
import parser.ast.expressions._
import typeinference._
import typeinference.TypeInference.TypeError
import org.junit._
import Assert._

class TypeInferenceTest {


  @Test (expected=classOf[TypeError])
  def test_Match_ReturnValue_Fail = {
    // \x -> match x with
    //        (y:ys) -> y 
    //        (y:[]) -> x
    val e14 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("y")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("x"))), patterns.Id("x"))
    assertEquals((TypeList(TypeVariable(2)),(List(TypeVariable(2)),TypeFn(TypeVariable(2),TypeVariable(2)))),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e14))
  }

  @Test
  def test_Match = {
    // \x -> match x with
    //        (y:ys) -> ys
    //        (y:[]) -> x
    val e15 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("ys")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Id("x"))), patterns.Id("x"))
    assertEquals((List(TypeVariable(3)),TypeFn(TypeList(TypeVariable(3)),TypeList(TypeVariable(3)))),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e15))
  }

  @Test
  def test_Pattern_Match_With_Nil = {
    // \x -> match x with
    //         (y:ys) -> ys
    //         (y:[]) -> x
    val e20 = Lambda(Match(Id("x"),
                           (patterns.Cons(patterns.Id("y"),patterns.Id("ys")), Id("ys")),
                           (patterns.Cons(patterns.Id("y"),patterns.Nil), Nil)), patterns.Id("x"))
    assertEquals((List(TypeVariable(3)),TypeFn(TypeList(TypeVariable(3)),TypeList(TypeVariable(3)))),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e20))
  }

  
  // TODO
  def test_Alternative_Pattern = {
    val todo = Lambda(Id("x"), patterns.Alternative(patterns.Cons(patterns.Id("x"),patterns.Id("xs")),
						    patterns.Cons(patterns.Id("x"),patterns.Nil)))
  }
  
  /**
   * More tests!
   */
  @Test
  def test_e1 = {
    val e1 = App(Lambda(Id("a"),patterns.Id("a")),Integer(42))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e1))
  }

  @Test
  def test_e2 = {
    val e2 = App(Lambda(BinOp(BinaryOperator.add,Id("a"),Integer(3)),patterns.Id("a")),Integer(42))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e2))
  }

  @Test
  def test_e4 = {
    val e3 = Let(patterns.Id("a"), Lambda(Id("a"), patterns.Id("a")),
		 App(Id("a"),Id("a")))
    val e4 = App(e3,Integer(42))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e4))
  }

  @Test
  def test_e5 = {
    val e5 = Lambda(Lambda(Lambda(
	  App(App(Id("a"),Id("b")), App(Id("b"), Id("c"))), patterns.Id("c")), patterns.Id("b")), patterns.Id("a"))
    assertEquals((List(TypeVariable(3), TypeVariable(6), TypeVariable(4)),
		  TypeFn(TypeFn(TypeFn(TypeVariable(3),TypeVariable(6)),TypeFn(TypeVariable(6),TypeVariable(4))),TypeFn(TypeFn(TypeVariable(3),TypeVariable(6)),TypeFn(TypeVariable(3),TypeVariable(4))))),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e5))

  }

  @Test
  def test_e6 = {
    // let a = \b.b in let b = \a.a in \a.a
    val e6 = Let(patterns.Id("a"),
		 Lambda(Id("b"), patterns.Id("b")),
		 Let(patterns.Id("b"),
		     App(Id("a"), Id("a")),
		     App(Id("a"), Id("a"))))
    assertEquals((List(TypeVariable(7)),TypeFn(TypeVariable(7),TypeVariable(7))),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e6))
  }

  @Test
  def test_e7 = {
    val e6 = Let(patterns.Id("a"),
		 Lambda(Id("b"), patterns.Id("b")),
		 Let(patterns.Id("b"),
		     App(Id("a"), Id("a")),
		     App(Id("a"), Id("a"))))
    // (let a = \b.b in let b = \a.a in \a.a) 'a'
    // EXPECTED RESULT 97
    val e7 = App(e6,Character('a'))
    assertEquals((List(),TypeChar()), TypeInference.typeCheck(TypeInference.emptyEnv, e7))
  }

  @Test (expected=classOf[TypeError])
  def test_e8 = {
    val e8 = IfThenElse(BinOp(BinaryOperator.eq,Integer(97),Character('a')),Integer(42),Bool(false))
    assertEquals((List(),TypeBool()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e8))
  }

  @Test
  def test_e9 = {
    val e9 = Match(Integer(2),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))
    assertEquals((List(),TypeBool()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e9))
  }

  @Test
  def test_e10 = {
    val e10 = Match(Integer(4),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))
    assertEquals((List(),TypeBool()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e10))
  }

  @Test
  def test_e11 = {
    val e11 = Match(Integer(42),(patterns.Id("x"),Id("x")))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e11))
  }

  @Test
  def test_e12 = {
    val e12 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
		    (patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
		     BinOp(BinaryOperator.add,Id("x"),Id("y"))))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e12))
  }

  @Test
  def test_e13 = {
    val e13 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(3)),Tuple(Integer(5),Integer(3))),
		    (patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
		     Match(Id("y"),
			   (patterns.Tuple(patterns.Integer(2),patterns.Underscore),Id("x")),
			   (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
			    BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36)))))))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e13))
  }

  @Test
  def test_e14 = {
    // match (1,(2,4),(5,3)) with (x,z,y) -> match z with (2,x) -> x | (z,3) -> x + z + 36
    val e14 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(4)),Tuple(Integer(5),Integer(3))),
		    (patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Id("y")),
		     Match(Id("z"),
			   (patterns.Tuple(patterns.Integer(2),patterns.Id("x")),Id("x")),
			   (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
			    BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36)))))))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e14))
  }

  @Test
  def test_e15 = {
    val e15 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
		    (patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Integer(6)),
		     Integer(5)),
		    (patterns.Tuple(patterns.Integer(1),patterns.Integer(2),patterns.Id("y")),
		     Id("y")))
    assertEquals((List(),TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e15))
  }

  @Test
  def test_BinOp_var1 = {
    val test = BinOp(BinaryOperator.add, Integer(1), Integer(2))

    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test
  def test_BinOp_var2 = {
    val test = BinOp(BinaryOperator.and, Bool(true), Bool(false))

    assertEquals((List(), TypeBool()), TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test (expected=classOf[TypeError])
  def test_BinOp_var3 {
    val test = BinOp(BinaryOperator.add, Integer(1), Bool(true))

    TypeInference.typeCheck(TypeInference.emptyEnv, test)
  }

  @Test (expected=classOf[TypeError])
  def test_BinOp_var4 {
    val test = BinOp(BinaryOperator.and, Integer(1), Bool(true))

    TypeInference.typeCheck(TypeInference.emptyEnv, test)
  }

  @Test
  def test_TupleElem = {
    val tuple = Tuple(Integer(1), Character('a'))
    assertEquals((List(),TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, TupleElem(tuple, 1)))
    assertEquals((List(),TypeChar()), TypeInference.typeCheck(TypeInference.emptyEnv, TupleElem(tuple, 2)))
  }

  @Test
  def test_Fac = {
    val fac = LetRec(App(Id("fac"), Integer(5)),
		     (patterns.Id("fac"),
		      Lambda(IfThenElse(BinOp(BinaryOperator.eq, Id("n"), Integer(0)),
					Integer(1),
					BinOp(BinaryOperator.mul, Id("n"), App(Id("fac"), BinOp(BinaryOperator.sub, Id("n"), Integer(1))))),
			     patterns.Id("n"))))
    assertEquals((List(), TypeInt()), TypeInference.typeCheck(TypeInference.emptyEnv, fac))
  }

  @Test
  def test_Ackermann = {
    val test = LetRec(App(Id("a"), Integer(1), Integer(2)),
		      (patterns.Id("a"),
		       Lambda(Lambda(Match(Id("m"),
					   (patterns.Integer(0), BinOp(BinaryOperator.add, Id("n"), Integer(1))),
					   (patterns.Underscore, Match(Id("n"),
								       (patterns.Integer(0), App(Id("a"), BinOp(BinaryOperator.sub, Id("m"), Integer(1)), Integer(1))),
								       (patterns.Underscore, App(Id("a"), BinOp(BinaryOperator.sub, Id("m"), Integer(1)),
												 App(Id("a"), Id("m"), BinOp(BinaryOperator.sub, Id("n"), Integer(1)))))))),
				     patterns.Id("m")),
			      patterns.Id("n"))))

    assertEquals((List(), TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, test))
  }

  @Test
  def test_Lambda_Match_Record = {
    val e = Let(patterns.Record((patterns.Id("field1"), patterns.Cons(patterns.Id("x"),patterns.Id("xs")))),
		expressions.Record((Id("field1"), Cons(Integer(1), Cons(Integer(2), Nil)))),
		Id("x"))
    assertEquals((List(), TypeInt()),
		 TypeInference.typeCheck(TypeInference.emptyEnv, e))
  }

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

