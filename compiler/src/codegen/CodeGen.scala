
package codegen.mama

import parser.ast._
import parser.ast.expressions._
import mamaInstructions._
import scala.collection.immutable.HashMap
import scala.collection.mutable.LinkedHashSet

package mamaInstructions {

	/******************************************************************************/
	/*														INSTRUCTION SET																	*/
	/******************************************************************************/
	sealed abstract class Instruction(str: String) {
		final override def toString() = str + (this match {
				case SETLABEL(_) => "\n"
				case LABEL(_) => ""
				case _ => "\n"
			})
	}
	final case class LABEL(nr:Int) extends Instruction("_" + nr.toString) with Ordered[LABEL] {
		override def compare(that: LABEL) = nr compare that.nr
	}
	final case class SETLABEL(label:LABEL) extends Instruction(label + ":")

	case object ADD extends Instruction("add") 
	final case class ALLOC(value:Int) extends Instruction("alloc " + value)
	case object AND extends Instruction("and")
	case object APPLY extends Instruction("apply")
	// case object APPLY0 extends Instruction("apply0") TODO necessary?
	case object CONS extends Instruction("cons")
	case object COPYGLOB extends Instruction("copyglob")	
	case object DIV extends Instruction("div")
	case object EQ extends Instruction("eq")
	final case class EVAL(label:LABEL) extends Instruction("eval")
	case object GEQ extends Instruction("geq")
	final case class GET(offset:Int) extends Instruction("get " + offset)
	case object GETBASIC extends Instruction("getbasic")
	final case class GETVEC(locvars:Int) extends Instruction("getvec" + locvars)
	case object GR extends Instruction("gr")
	case object HALT extends Instruction("halt")
	final case class JUMP(target:LABEL) extends Instruction("jump " + target)
	final case class JUMPZ(target:LABEL) extends Instruction("jumpz " + target)
	case object LE extends Instruction("le")
	case object LEQ extends Instruction("leq")
	final case class LOADC(value:Int) extends Instruction("loadc " + value)
	final case class MARK(ret:LABEL) extends Instruction("mark " + ret)
	// case object MARK0 extends Instruction("mark0") TODO necessary?
	case object MKBASIC extends Instruction("mkbasic")
	final case class MKCLOS(target:LABEL) extends Instruction("mkclos " + target)
	final case class MKFUNVAL(target:LABEL) extends Instruction("mkfunval " + target)
	final case class MKVEC(length:Int) extends Instruction("mkvec " + length)
	// case object MKVEC0 extends Instruction("mkvec0") TODO necessary?
	case object MUL extends Instruction("mul")
	case object NEG extends Instruction("neg")
	case object NEQ extends Instruction("neq")
	case object NIL extends Instruction("nil")
	case object NOT extends Instruction("not")
	case object OR extends Instruction("or")
	case object POP extends Instruction("pop")
	case object POPENV extends Instruction("popenv")
	final case class PUSHGLOB(value:Int) extends Instruction("pushglob " + value)
	final case class PUSHLOC(value:Int) extends Instruction("pushloc " + value)
	final case class RETURN(drop:Int) extends Instruction("return " + drop)
	final case class REWRITE(depth:Int) extends Instruction("rewrite " + depth)
	final case class SLIDE(drop:Int) extends Instruction("slide " + drop)
	case object SUB extends Instruction("sub")
	final case class TARG(drop:Int, label:LABEL) extends Instruction("targ " + drop)
	final case class TLIST(target:LABEL) extends Instruction("tlist " + target)
	case object UPDATE extends Instruction("update")
	final case class WRAP(label:LABEL) extends Instruction("wrap")
}

object Translator {
	
	object VarKind extends Enumeration {
		type VarKind = Value
		val Global, Local = Value
	}
	
	var counter = 1 // kind of ugly, because not functional
		
	val CBN = true // call-by-name / call-by-value switch
	val cbfun = if(CBN) codec _ else codev _ 
		
	/******************************************************************************/
	/*																	CODEGEN																		*/
	/******************************************************************************/
	// on-stack computations for mama
	def codeb(expr:Expression, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
	:List[Instruction] = expr match {
		case Integer(x) => List(LOADC(x))
		case Bool(x) => List(LOADC(if(x) 1 else 0))
		case Character(x) => List(LOADC(char2int(x)))
		case Id(x) => getvar(x, rho, sd) ++
			(if(CBN) { val A = newLabel(); List(EVAL(A), SETLABEL(A))} else List.empty) :+ GETBASIC
		case UnOp(op,e) => 
			codeb(e,rho,sd) :+ instrOfOp(op)
		case BinOp(op,e1,e2) => 
			codeb(e1,rho,sd) ++ codeb(e2,rho,sd+1) :+ instrOfOp(op)
		case IfThenElse(c,e1,e2) => {
				val A = newLabel()
				val B = newLabel()
				(codeb(c, rho, sd) ++ List(JUMPZ(A)) 
				 ++ codeb(e1, rho, sd) ++ List(JUMP(B),SETLABEL(A))
				 ++ codeb(e2, rho, sd) :+ SETLABEL(B))
			}
		case _ => codev(expr, rho, sd) ++
			(if(CBN) { val A = newLabel(); List(EVAL(A), SETLABEL(A))} else List.empty) :+ GETBASIC
	}
			
	def codec(expr:Expression, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
	:List[Instruction] = {
		val zs = (free(expr,LinkedHashSet.empty)).toList
		val globs = (0 to zs.size-1).toList
			
		val rhoNew = (globs foldLeft rho)(updateGlob(zs,_,_))
			
		val A = newLabel()
		val B = newLabel()
			
		((globs foldLeft (List.empty:List[Instruction]))((l:List[Instruction],i:Int) => 
				l ++ getvar((Id unapply zs(i)).get,rho,sd+i))) ++
		List(MKVEC(globs.length),MKCLOS(A),JUMP(B),SETLABEL(A)) ++ codev(expr,rhoNew,0) ++ 
		List(UPDATE,SETLABEL(B))
	}
			
	def codev(expr:Expression, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
	:List[Instruction] = expr match {
		case Integer(x) => List(LOADC(x),MKBASIC)
		case Bool(x) => List(LOADC(if(x) 1 else 0),MKBASIC)
		case Character(x) => List(LOADC(char2int(x)),MKBASIC)
		case Id(x) => getvar(x, rho, sd) ++
			(if(CBN) { val A = newLabel(); List(EVAL(A), SETLABEL(A))} else List.empty)
		case UnOp(op,e) => 
			codeb(e,rho,sd) ++ List(instrOfOp(op),MKBASIC)
		case BinOp(op,e1,e2) => 
			codeb(e1,rho,sd) ++ codeb(e2,rho,sd+1) ++ List(instrOfOp(op),MKBASIC)
		case IfThenElse(c,e1,e2) => {
				val A = newLabel()
				val B = newLabel()
				codeb(c, rho, sd) ++ List(JUMPZ(A)) ++ 
				codev(e1, rho, sd) ++ List(JUMP(B),SETLABEL(A)) ++ 
				codev(e2, rho, sd) :+ SETLABEL(B)
			}
		case Let(x,definition,body) => x match {
				case patterns.Id(x) => cbfun(definition, rho, sd) ++ 
					codev(body, rho + (x -> (VarKind.Local,sd+1)), sd+1) :+ SLIDE(1)
					// let complex_pat = def in body = match def with complex_pat -> body
				case _ => codev(Match(definition,(x,body)), rho, sd)
			}
		case LetRec(body,patDef@_*) => {
				val n = patDef.length
				val list = (0 to n-1).toList
				def varI(i:Int):String = patDef(i) _1 match { 
					case patterns.Id(x) => x
				}
				def update(r:HashMap[String,(VarKind.Value,Int)],i:Int)
				:HashMap[String,(VarKind.Value,Int)] = r + (varI(i) -> (VarKind.Local,sd+i+1))
				val rhoNew = (list foldLeft rho)(update _)
					
				ALLOC(n) +:
				(list.flatMap((i:Int)=>cbfun(patDef(i) _2, rhoNew, sd+n) :+ REWRITE(n-i)) ++
				 codev(body,rhoNew,sd+n) :+ SLIDE(n))
			}
		case Lambda(body,args@_*) => if(args forall { case patterns.Id(_) => true case _ => false }) {
				val zs = (free(expr,LinkedHashSet.empty)).toList
				val locs = (0 to args.length-1).toList
				val globs = (0 to zs.size-1).toList
					
				val rhoNew = 
					(globs foldLeft ((locs foldLeft rho)(updateLoc(args.toList,_,_))))(updateGlob(zs,_,_))	
						
				val A = newLabel()
				val B = newLabel()
					
				((globs foldLeft (List.empty:List[Instruction]))((l:List[Instruction],i:Int) => 
						l ++ getvar((Id unapply zs(i)).get,rho,sd+i))) ++
				List(MKVEC(globs.length),MKFUNVAL(A),JUMP(B),SETLABEL(A),TARG(locs.length,A)) ++ 
				codev(body,rhoNew,0) ++ List(RETURN(locs.length), SETLABEL(B))
			} else { // \complex_pat.body = \x.match x with complex_pat -> body
				counter = counter + args.size
				val list = (1 to args.size).toList
				val matchExpr = (list foldLeft body)
				{ case (e,i) => Match(Id("42"+(counter + i)),(args(i-1),e)) }
				val newExpr = Lambda(matchExpr,(list map { (i:Int) => patterns.Id("42"+(counter + i)) }):_*)
				
				codev(newExpr,rho,sd)
			}
		case App(fun,args@_*) => {
				val A = newLabel()
				val m = args.length
				val is = (0 to m - 1).toList
					
				(is foldLeft (List(MARK(A)):List[Instruction]))((l:List[Instruction],i:Int) => 
					l ++ cbfun(args(m-1-i),rho,sd+3+i)) ++ codev(fun,rho,sd+3+m) ++ List(APPLY,SETLABEL(A))
			}
		case Tuple(elems@_*) => {
				val ks = (0 to elems.length-1).toList
					
				(ks foldLeft (List.empty:List[Instruction]))((l:List[Instruction],k:Int) => 
					l ++ codec(elems(k),rho,sd+k)) :+ MKVEC(ks.length)
			}
		case TupleElem(t,i) => codev(t,rho,sd) :+ GET(i)
		case Nil => List(NIL)
		case Cons(head,tail) => cbfun(head,rho,sd) ++ cbfun(tail,rho,sd+1) :+ CONS
			// FIXME For now ignore e1 - this will be changed when side effects are allowed
		case Sequence(e1,e2) => codev(e2,rho,sd) 
		case Record(idVals@_*) => {
				val ks = (0 to idVals.length-1).toList
					
				(ks foldLeft (List.empty:List[Instruction]))((l:List[Instruction],k:Int) => 
					l ++ codec(idVals(k) _2,rho,sd+k)) :+ MKVEC(ks.length)
			}
			// FIXME get position from type checking
		case Field(rec,name) => codev(rec,rho,sd) :+ GET(0/*getRecordLabelPos(rec,name)*/) 
		case Match(e0,patDefs@_*) => patDefs match {
				case Seq((patterns.Nil,e1:Expression),(patterns.Cons(patterns.Id(h),patterns.Id(t)),e2:Expression))
					=> {
						val A = newLabel()
						val B = newLabel()
					 
						(codev(e0,rho,sd) ++ List(TLIST(A)) ++ codev(e1,rho,sd) ++ List(JUMP(B),SETLABEL(A)) ++
						 codev(e2,rho + {h -> (VarKind.Local,sd+1)} + {t -> (VarKind.Local,sd+2)},sd+2) ++ 
						 List(SLIDE(2),SETLABEL(B)))
					}			
				case _ => {
						val DONE = newLabel()
						val E = "42" + (counter + 1)//not a valid identifier -> no conflicts
						counter = counter + 1
						val rhoNew = rho + {E -> (VarKind.Local,sd+1)} // store value of e0
							
						codev(e0,rhoNew,sd) ++ (patDefs.toList flatMap { case (p,e) => {
										val NEXT = newLabel()
										val (matchingCode,n) = matchCG(e0,p,e,NEXT,DONE,E,rhoNew,sd+1)
											
										matchingCode ++ List(SETLABEL(NEXT)) ++ pop(n) 
									}
							/* HALT is misused for runtime pattern match errors */
							}) ++ List(HALT,SETLABEL(DONE))
					}
			}
	}
		
	/******************************************************************************/
	/*																	MACROS																		*/
	/******************************************************************************/
	def matchCG(e0:Expression, p:patterns.Pattern, res:Expression, NEXT:LABEL, DONE:LABEL, E:String,
							rho:HashMap[String,(VarKind.Value,Int)],sd:Int):(List[Instruction],Int) = {
			
		val vars = bound(p)
		val n = vars.size
		val rhoNew = ((1 to n).toList foldLeft rho)
		{case (rhoTemp,i) => 
				rhoTemp + {((Id unapply (vars.toList)(i-1)).get) -> (VarKind.Local,sd+i)}}
		
		def matching(e:Expression, p:patterns.Pattern, sdAct:Int, i:Int):(List[Instruction],Int,Int) =
			p match {
				case patterns.Integer(n) => (codeb(BinOp(BinaryOperator.eq,Integer(n),e),rhoNew,sdAct),sdAct+1,i)
				case patterns.Character(a) => (codeb(BinOp(BinaryOperator.eq,Character(a),e),rhoNew,sdAct),sdAct+1,i)
				case patterns.Bool(b) => (codeb(BinOp(BinaryOperator.eq,Bool(b),e),rhoNew,sdAct),sdAct+1,i)
				case patterns.Underscore => (List(LOADC(1)),sdAct+1,i)
				case patterns.Id(x) => (codev(e,rhoNew,sdAct) ++ List(REWRITE(i+(sdAct-(sd+n))),LOADC(1)),sdAct+1,i-1)
				case patterns.Tuple(ts@_*) => 
					((1 to ts.length - 1).toList foldLeft matching(TupleElem(Id(E),0),ts(0),sdAct,i)){
						case ((l,sd,j),n) => {
								val (lt,sdNew,jNew) = matching(TupleElem(Id(E),n),ts(n),sd,j)

								(l ++ lt :+ AND,sdNew-1,jNew)
							}
					}
				case patterns.Record((patterns.Id(x),exp),ts@_*) => 
					(ts.toList foldLeft matching(Field(Id(E),Id(x)),exp,sdAct,i)){
						case ((l,sd,j),idExp) => {
								val (lt,sdNew,jNew) = 
									matching(Field(Id(E),Id((patterns.Id unapply(idExp _1)).get)),idExp _2,sd,j)

								(l ++ lt :+ AND,sdNew-1,jNew)
							}
					}
					// TODO Reconsider this
				case patterns.Alternative(p1,p2) => {
						if (!(bound(p1) ++ bound(p2)).isEmpty) 
							throw new Exception("A pattern with \"|\" must not bind variables.")
							
						val (l1,sdAct1,i1) = matching(e,p1,sdAct,i)
						val (l2,sdAct2,i2) = matching(e,p2,sdAct1,i1)
							
						(l1 ++ l2 :+ OR,sdAct2-1,i2)
					}
					// This is UGLY
				case patterns.Nil => {
						val x = (counter + 1).toString
						val y = (counter + 2).toString
						counter = counter + 2
						
						(codeb(Match(e.subst(e0, Id(E)),(patterns.Nil,Bool(true)),
												 (patterns.Cons(patterns.Id("42"+x),patterns.Id("42"+y)),Bool(false))),
									 rhoNew,sdAct),sdAct+1,i)
					}
				case patterns.Cons(head,tail) => {
						val x = (counter + 1).toString
						val y = (counter + 2).toString
						val z = (counter + 3).toString
						counter = counter + 3
						
						val (l1,sdAct1,i1) = 
							matching(Match(e.subst(e0, Id(E)),(patterns.Nil,Bool(false)),
														 (patterns.Cons(patterns.Id("42"+x),patterns.Id("42"+z)),Id("42"+x))),
											 head,sdAct,i)
						val (l2,sdAct2,i2) = 
							matching(Match(e.subst(e0, Id(E)),(patterns.Nil,Bool(false)),
														 (patterns.Cons(patterns.Id("42"+z),patterns.Id("42"+y)),Id("42"+y))),
											 tail,sdAct1,i1)
						
							
						(l1 ++ l2 :+ AND,sdAct2-1,i2)
					}
			}

		val (matchingCode,sdNew,_) = matching(e0,p,sd+n,n)
			
    (List(ALLOC(n)) ++ matchingCode ++ List(JUMPZ(NEXT)) ++
		 codev(res,rhoNew,sd+n) ++ List(SLIDE(n+1),JUMP(DONE)),n)
		
	}
	
	def pop(n:Int):List[Instruction] = n match {
    case 0 => List.empty
    case 1 => List(POP)
    case _ => List(SLIDE(n-1),POP)
  }
		
	def getvar(x:String, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
	:List[Instruction] = rho.get(x) match {
		case Some((VarKind.Global,i)) => List(PUSHGLOB(i))
		case Some((VarKind.Local,i)) => {/*Console println (x,rho,sd,sd-i);*/ List(PUSHLOC(sd-i)) }
		case _ => throw new Exception("Undefined variable " + x + " in codegen-phase")
	}
		
	// Finds the instruction corresponding to the given operator
	def instrOfOp(op:Operator#Value):Instruction = op match {
		case BinaryOperator.add => ADD
		case BinaryOperator.sub => SUB
		case BinaryOperator.mul => MUL
		case BinaryOperator.div => DIV
		case BinaryOperator.eq => EQ
		case BinaryOperator.neq => NEQ
		case BinaryOperator.geq => GEQ
		case BinaryOperator.leq => LEQ
		case BinaryOperator.gr => GR
		case BinaryOperator.le => LE
		case BinaryOperator.and => AND
		case BinaryOperator.or => OR
		case UnaryOperator.neg => NEG
		case UnaryOperator.not => NOT
	}
		
	def newLabel():LABEL = {
		counter = counter + 1 
		LABEL(counter)
	}
		
	def varTest(v:Any): String = v match { 
		case patterns.Id(x) => x
		case Id(x) => x
		case _ => throw new Exception("TODO" + v.toString)
	}
	def updateLoc(l:List[Any],r:HashMap[String,(VarKind.Value,Int)],i:Int)
	:HashMap[String,(VarKind.Value,Int)] = r + (varTest(l(i)) -> (VarKind.Local,-i))
	def updateGlob(l:List[Any],r:HashMap[String,(VarKind.Value,Int)],i:Int)
	:HashMap[String,(VarKind.Value,Int)] = r + (varTest(l(i)) -> (VarKind.Global,i))
			
	/******************************************************************************/
	/*														FREE & BOUND VARIABLES													*/
	/******************************************************************************/
	def free(e:Expression,vs:LinkedHashSet[Id]):LinkedHashSet[Id] = e match {
		case x@Id(_) => if (vs contains x) LinkedHashSet.empty else LinkedHashSet(x)
		case UnOp(_,e) => free(e,vs)
		case BinOp(_,e1,e2) => free(e1,vs) ++ free(e2,vs)
		case IfThenElse(c,e1,e2) => free(c,vs) ++ free(e1,vs) ++ free(e2,vs)
		case App(f,args@_*) => 
			args.toList.foldLeft(free(f,vs))((s:LinkedHashSet[Id],arg:Expression) => s ++ free(arg,vs))
		case Lambda(body,args@_*) => free(body, vs ++ ((LinkedHashSet.empty ++ args) flatMap bound))
		case Let(pat,_,expr) => free(expr, vs ++ bound(pat))
		case LetRec(body,patDef@_*) => {
				val (pats,defs) = (LinkedHashSet.empty ++ patDef).unzip
				val vsNew = vs ++ (pats flatMap bound)
				(defs + body) flatMap ((e:Expression) => free(e,vsNew))
			}
		case Match(exp,patCase@_*) => {
				val (pats,cases) = (LinkedHashSet.empty ++ patCase).unzip
				val vsNew = vs ++ (pats flatMap bound)
				(cases + exp) flatMap ((e:Expression) => free(e,vsNew))
			}
		case Tuple(es@_*) => LinkedHashSet.empty ++ es.toList flatMap (free(_:Expression,vs))
		case TupleElem(t,_) => free(t,vs)
		case Record(idExp@_*) => LinkedHashSet.empty ++ idExp.toList flatMap { case (_,exp) => free(exp,vs)}
		case Field(rec,_) => free(rec,vs)
		case Cons(h,t) => free(h,vs) ++ free(t,vs)
		case _ => LinkedHashSet.empty
	}
		
	def bound(pat:patterns.Pattern):LinkedHashSet[Id] = pat match {
		case patterns.Id(x) => LinkedHashSet(Id(x))
		case patterns.Record(pats@_*) => LinkedHashSet.empty ++
			pats flatMap ((x:(patterns.Id, patterns.Pattern)) => bound(x _1) ++ bound(x _2))
		case patterns.Cons(x,xs) => bound(x) ++ bound(xs)
		case patterns.Alternative(a,b) => bound(a) ++ bound(b)
		case patterns.Tuple(ts@_*) => LinkedHashSet.empty ++ ts flatMap bound
		case _ => LinkedHashSet.empty
	}
}

object CodeGen {
		
	def main(args:Array[String]):Unit = {
		// let a = 19 in let b = a * a in a + b
		// EXPECTED RESULT 380
		val e0 = Let(patterns.Id("a"),Integer(19),
								 Let(patterns.Id("b"),BinOp(BinaryOperator.mul,Id("a"),Id("a")),
										 BinOp(BinaryOperator.add,Id("a"),Id("b"))
			)
		)
		
		// (\a.a) 42
		// EXPECTED RESULT 42
		val e1 = App(Lambda(Id("a"),patterns.Id("a")),Integer(42))
		
		// (\a.a + 3) 42
		// EXPECTED RESULT 45
		val e2 = App(Lambda(BinOp(BinaryOperator.add,Id("a"),Integer(3)),patterns.Id("a")),Integer(42))
		
		// let a = \a.a in a a
		// EXPECTED RESULT NOT BASIC ERROR 
		val e3 = Let(patterns.Id("a"), Lambda(Id("a"), patterns.Id("a")),
								 App(Id("a"),Id("a")))
		
		// (let a = \a.a in a a) 42 
		// EXPECTED RESULT 42
		val e4 = App(e3,Integer(42))

		// \a.(\b.(\c.(a b) (b c)))
		// EXPECTED RESULT NOT BASIC ERROR
		val e5 = Lambda(Lambda(Lambda(
					App(App(Id("a"),Id("b")), App(Id("b"), Id("c"))), patterns.Id("c")), patterns.Id("b")), patterns.Id("a"))

		// let a = \b.b in let b = \a.a in \a.a
		// EXPECTED RESULT NOT BASIC ERROR
		val e6 = Let(patterns.Id("a"),
								 Lambda(Id("b"), patterns.Id("b")),
								 Let(patterns.Id("b"),
										 App(Id("a"), Id("a")),
										 App(Id("a"), Id("a"))))
	 
		// (let a = \b.b in let b = \a.a in \a.a) 'a'
		// EXPECTED RESULT 97
		val e7 = App(e6,Character('a'));
		
		// if 97 = 'a' then 42 else false
		// EXPECTED RESULT 42
		val e8 = IfThenElse(BinOp(BinaryOperator.eq,Integer(97),Character('a')),Integer(42),Bool(false))
		
		// match 2 with 3 -> true | 2 -> false
		// EXPECTED RESULT 0
		val e9 = Match(Integer(2),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))
		
		// match 4 with 3 -> true | 2 -> false
		// EXPECTED RESULT SOME ERROR
		val e10 = Match(Integer(4),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))
		
		// match 42 with x -> x
		// EXPECTED RESULT 42
		val e11 = Match(Integer(42),(patterns.Id("x"),Id("x")))
		
		// match (1,2,3) with (x,_,y) -> x + y
		// EXPECTED RESULT 4
		val e12 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
										(patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
										 BinOp(BinaryOperator.add,Id("x"),Id("y")))
		)
		
		// match (1,(2,3),(5,3)) with (x,_,y) -> match y with (2,_) -> x | (z,3) -> x + z + 36 
		// EXPECTED RESULT 42
		val e13 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(3)),Tuple(Integer(5),Integer(3))),
										(patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
										 Match(Id("y"),
													 (patterns.Tuple(patterns.Integer(2),patterns.Underscore),Id("x")),
													 (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
														BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36))))))
		)
		
		// match (1,(2,4),(5,3)) with (x,z,y) -> match z with (2,x) -> x | (z,3) -> x + z + 36
		// EXPECTED RESULT 4
		val e14 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(4)),Tuple(Integer(5),Integer(3))),
										(patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Id("y")),
										 Match(Id("z"),
													 (patterns.Tuple(patterns.Integer(2),patterns.Id("x")),Id("x")),
													 (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
														BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36))))))
		)

		// match (1,2,3) with (x,z,6) -> 5 | (1,2,y) -> y
		// EXPECTED RESULT 3
		val e15 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
										(patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Integer(6)),
										 Integer(5)),
										(patterns.Tuple(patterns.Integer(1),patterns.Integer(2),patterns.Id("y")),
										 Id("y"))
		)

    // (\x::xs.x) [42,5]
    // EXPECTED RESULT 42
		val e16 = App(Lambda(Id("x"), patterns.Cons(patterns.Id("x"), patterns.Id("xs"))),
                Cons(Integer(42),Cons(Integer(5),Nil)))

    // (\y::ys.\x::xs.x) [3] [42,5]
    // EXPECTED RESULT 42
		val e17 = App(App(Lambda(Lambda(Id("x"), patterns.Cons(patterns.Id("x"), patterns.Id("xs"))),
										 patterns.Cons(patterns.Id("y"), patterns.Id("ys"))),Cons(Integer(3),Nil)),
        Cons(Integer(42),Cons(Integer(5),Nil)))
		
		val list = List(e0,e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17)	
			
		def out(e:Expression):Unit = {
			val is = Translator.codeb(e,HashMap.empty,0)
			val out = new java.io.FileWriter("e" + (list indexOf e).toString + ".mama")
			is map ((i:Instruction) => out write (i.toString))
			out close
		}
		
		list map out
	}
}
