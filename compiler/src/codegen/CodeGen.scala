
package codegen.mama

package mamaInstructions {
	import parser.ast._
	import parser.ast.expressions._
	import scala.collection.immutable.HashMap
	import scala.collection.immutable.ListSet

	object VarKind extends Enumeration {
		type VarKind = Value
		val Global, Local = Value
	}

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
	final case class LABEL(nr:Int) extends Instruction("_" + nr.toString)
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
	// case object GETREF extends Instruction("getref") TODO necessary?
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
	// case object MKREF extends Instruction("mkref") TODO necessary?
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
	case object STORE extends Instruction("store")
	case object SUB extends Instruction("sub")
	final case class TARG(drop:Int, label:LABEL) extends Instruction("targ " + drop)
	final case class TLIST(target:LABEL) extends Instruction("tlist " + target)
	case object UPDATE extends Instruction("update")
	case object WRAP extends Instruction("wrap")
	case object XOR extends Instruction("xor")
	
	object Translator {
		var counter = 0 // kind of ugly, because not functional
		
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
			case Id(x) => getvar(x, rho, sd) :+ GETBASIC
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
			case _ => codev(expr, rho, sd) :+ GETBASIC
		}
			
		def codec(expr:Expression, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
		:List[Instruction] = {
			val zs = (free(expr,ListSet.empty)).toList
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
			case Id(x) => getvar(x, rho, sd)
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
					case _ => throw new Exception("TODO")
				}
			case LetRec(body,patDef@_*) => {
					val n = patDef.length
					val list = (0 to n-1).toList
					def varI(i:Int):String = patDef(i) _1 match { 
						case patterns.Id(x) => x
						case _ => throw new Exception("TODO")
					}
					def update(r:HashMap[String,(VarKind.Value,Int)],i:Int)
					:HashMap[String,(VarKind.Value,Int)] = r + (varI(i) -> (VarKind.Local,sd+i))
					val rhoNew = (list foldLeft rho)(update _)
					
					ALLOC(n) +:
					(list.flatMap((i:Int)=>cbfun(patDef(i) _2, rhoNew, sd+n) :+ REWRITE(n-i+1)) ++
					 codev(body,rhoNew,sd+n) :+ SLIDE(n))
				}
			case Lambda(body,args@_*) => {
					val zs = (free(expr,ListSet.empty)).toList
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
			case Field(rec,name) => codev(rec,rho,sd) ++ List(GET(0/*getPos(rec,name)*/)) 
			case Match(e0,patDefs@_*) => codev(e0,rho,sd) ++ patDefs match {
					case Seq((patterns.Nil,e1:Expression),(patterns.Cons(patterns.Id(h),patterns.Id(t)),e2:Expression))
						=> {
							val A = newLabel()
							val B = newLabel()
					 
							(List(TLIST(A)) ++ codev(e1,rho,sd) ++ List(JUMP(B),SETLABEL(A)) ++
							 codev(e2,rho + {h -> (VarKind.Local,sd+1)} + {t -> (VarKind.Local,sd+2)},sd+2) ++ 
							 List(SLIDE(2),SETLABEL(B)))
						}			
					case _ => patDefs.toList flatMap { case (p,e) => matchCG(p,e,rho,sd) } //work in progress
				}
		}
		
		/******************************************************************************/
		/*																	MACROS																		*/
		/******************************************************************************/
		def matchCG(p:patterns.Pattern, res:Expression, 
								rho:HashMap[String,(VarKind.Value,Int)],sd:Int):List[Instruction] = {
			val A = newLabel()
			val B = newLabel()
			
			List(JUMPZ(B)) ++ codev(res,rho,sd) :+ SETLABEL(B)
		}
		
		def getvar(x:String, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
		:List[Instruction] = (rho.get(x) match {
				case Some((VarKind.Global,i)) => PUSHGLOB(i)
				case Some((VarKind.Local,i)) => PUSHLOC(sd-i)
				case _ => throw new Exception("Undefined variable in codegen-phase")
			}) +: (if(CBN) { val A = newLabel(); List(EVAL(A), SETLABEL(A))} else List.empty)
		
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
		def free(e:Expression,vs:ListSet[Id]):ListSet[Id] = e match {
			case x@Id(_) => if (vs contains x) ListSet.empty else ListSet(x)
			case UnOp(_,e) => free(e,vs)
			case BinOp(_,e1,e2) => free(e1,vs) ++ free(e2,vs)
			case IfThenElse(c,e1,e2) => free(c,vs) ++ free(e1,vs) ++ free(e2,vs)
			case App(f,args@_*) => 
				args.toList.foldLeft(free(f,vs))((s:ListSet[Id],arg:Expression) => s ++ free(arg,vs))
			case Lambda(body,args@_*) => free(body, vs ++ (args.toSet flatMap bound))
			case Let(pat,_,expr) => free(expr, vs ++ bound(pat))
			case LetRec(body,patDef@_*) => {
					val (pats,defs) = (ListSet.empty ++ patDef).unzip
					val vsNew = vs ++ (pats flatMap bound)
					(defs + body) flatMap ((e:Expression) => free(e,vsNew))
				}
			case _ => ListSet.empty
		}
		
		def bound(pat:patterns.Pattern):ListSet[Id] = pat match {
			case patterns.Id(x) => ListSet(Id(x))
			case patterns.Record(pats@_*) => ListSet.empty ++
				pats flatMap ((x:Tuple2[patterns.Id,patterns.Pattern]) => bound(x _1) ++ bound(x _2))
			case patterns.Cons(x,xs) => bound(x) ++ bound(xs)
			case patterns.Alternative(a,b) => bound(a) ++ bound(b)
			case patterns.Tuple(ts@_*) => ListSet.empty ++ ts flatMap bound
			case _ => ListSet.empty
		}
	}
}

object CodeGen {
	import parser.ast._
	import parser.ast.expressions._	
	import codegen.mama.mamaInstructions._	
	import scala.collection.immutable.HashMap
		
	def main(args:Array[String]):Unit = {
		// let a = 19 in let b = a * a in a + b
		val e0 = Let(patterns.Id("a"),Integer(19),
								 Let(patterns.Id("b"),BinOp(BinaryOperator.mul,Id("a"),Id("a")),
										 BinOp(BinaryOperator.add,Id("a"),Id("b"))
			)
		)
		
		// (\a.a) 42
		val e1 = App(Lambda(Id("a"),patterns.Id("a")),Integer(42))
		
		// (\a.a + 3) 42
		val e2 = App(Lambda(BinOp(BinaryOperator.add,Id("a"),Integer(3)),patterns.Id("a")),Integer(42))
		
		// let a = \a.a in a a 
		val e3 = Let(patterns.Id("a"), Lambda(Id("a"), patterns.Id("a")),
								 App(Id("a"),Id("a")))
		
		// (let a = \a.a in a a) 5 
		val e4 = App(e3,Integer(42))

		// \a.(\b.(\c.(a b) (b c)))
		val e5 = Lambda(Lambda(Lambda(
					App(App(Id("a"),Id("b")), App(Id("b"), Id("c"))), patterns.Id("c")), patterns.Id("b")), patterns.Id("a"))

		// let a = \b.b in let b = \a.a in \a.a
		val e6 = Let(patterns.Id("a"),
								 Lambda(Id("b"), patterns.Id("b")),
								 Let(patterns.Id("b"),
										 App(Id("a"), Id("a")),
										 App(Id("a"), Id("a"))))
	 
		// (let a = \b.b in let b = \a.a in \a.a) 'a'
		val e7 = App(e6,Character('a'));
	 
		val e8 = Lambda(Id("xs"), patterns.Cons(patterns.Id("x"), patterns.Id("xs")))
		val e9 = Lambda(Lambda(Id("xs"), patterns.Cons(patterns.Id("x"), patterns.Id("xs"))),
										patterns.Cons(patterns.Id("y"), patterns.Id("ys")))
		
		// match (1,2,3) with (1,2,6) -> 5 | (1,2,y) -> y
		val e10 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
										(patterns.Tuple(patterns.Integer(1),patterns.Integer(2),patterns.Integer(3)),
										 Integer(0)),
										(patterns.Tuple(patterns.Integer(1),patterns.Integer(2),patterns.Id("y")),
										 Id("y"))
		)
		
		val list = List(e0,e1,e2,e3,e4,e5,e6,e7,e10)	
			
		def out(e:Expression):Unit = {
			val is = Translator.codeb(e,HashMap.empty,0)
			val out = new java.io.FileWriter("e" + (list indexOf e).toString + ".mama")
			is map ((i:Instruction) => out write (i.toString))
			out close
		}
		
		list map out
	}
}
