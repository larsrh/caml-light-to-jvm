
package codegen.mama

package mamaInstructions {
  import parser.ast._
  import parser.ast.expressions._
  import scala.collection.immutable.HashMap

  object VarKind extends Enumeration {
     type VarKind = Value
     val Global, Local = Value
  }
  
  sealed abstract class Instruction(str: String) {
    final override def toString() = str + "\n"
  }
  final case class LABEL(name:String) extends Instruction(name + ":")
  
  case object ADD extends Instruction("add")
  case object AND extends Instruction("and")
  final case class ALLOC(value:Int) extends Instruction("alloc " + value)
  case object CALL extends Instruction("call")
  case object DIV extends Instruction("div")
  case object DUP extends Instruction("dup")
  final case class ENTER(value:Int) extends Instruction("enter " + value)
  case object EQ extends Instruction("eq")
  case object GEQ extends Instruction("geq")
  case object GR extends Instruction("gr")
  case object HALT extends Instruction("halt")
  final case class JUMP(target:LABEL) extends Instruction("jump " + target)
  final case class JUMPZ(target:LABEL) extends Instruction("jumpz " + target)
  final case class JUMPI(target:LABEL) extends Instruction("jumpi " + target)
  case object LE extends Instruction("le")
  case object LEQ extends Instruction("leq")
  case object LOAD extends Instruction("load")
  final case class LOADA(address:Int) extends Instruction("loada " + address)
  final case class LOADC(value:Int) extends Instruction("loadc " + value)
  final case class LOADR(address:Int, value:Int) extends Instruction("loadr " + address + " " + value)
  final case class LOADRC(value:Int) extends Instruction("loadrc " + value)
  final case class MALLOC(value:Int) extends Instruction("malloc " + value)
  case object MUL extends Instruction("mul")
  case object NEG extends Instruction("neg")
  case object NEQ extends Instruction("neq")
  case object NEW extends Instruction("new")
  case object OR extends Instruction("or")
  case object POP extends Instruction("pop")
  case object STORE extends Instruction("store")
  final case class STOREA(address:Int) extends Instruction("storea " + address)
  final case class STORER(address:Int, value:Int) extends Instruction("storer " + address + " " + value)
  case object SUB extends Instruction("sub")

  case object APPLY extends Instruction("apply")
  case object APPLY0 extends Instruction("apply0")
  case object CONS extends Instruction("cons")
  case object COPYGLOB extends Instruction("copyglob")
  //case object EVAL extends Instruction // TODO macro
  case object GETBASIC extends Instruction("getbasic")
  final case class GETVEC(locvars:Int) extends Instruction("getvec" + locvars)
  final case class MARK(ret:LABEL) extends Instruction("mark " + ret)
  case object MARK0 extends Instruction("mark0")
  case object MKBASIC extends Instruction("mkbasic")
  final case class MKCLOS(target:LABEL) extends Instruction("mkclos " + target)
  final case class MKFUNVAL(target:LABEL) extends Instruction("mkfunval " + target)
  final case class MKVEC(length:Int) extends Instruction("mkvec " + length)
  case object MKVEC0 extends Instruction("mkvec0")
  case object NIL extends Instruction("nil")
  final case class PUSHGLOB(value:Int) extends Instruction("pushglob " + value)
  final case class PUSHLOC(value:Int) extends Instruction("pushloc " + value)
  // final case class RETURN(drop:Int) extends Instruction // TODO macro
  final case class REWRITE(depth:Int) extends Instruction("rewrite" + depth)
  case object POPENV extends Instruction("popenv")
  final case class SLIDE(drop:Int) extends Instruction("slide" + drop)
  // final case class TARG(drop:Int) extends Instruction // TODO macro
  case object UPDATE extends Instruction("update")
  case object WRAP extends Instruction("wrap")
  
  object Translator {
    var counter = 0 // kind of ugly, because not functional 
    
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
            ++ codeb(e1, rho, sd) ++ List(JUMP(B),A)
            ++ codeb(e2, rho, sd) :+ B)
        }
        case _ => codev(expr, rho, sd) :+ GETBASIC
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
            codev(e1, rho, sd) ++ List(JUMP(B),A) ++ 
            codev(e2, rho, sd) :+ B
        }
        case Let(x,definition,body) => x match {
            // TODO first codev to be replaced by codec when CBN is used
          case patterns.Id(x) => codev(definition, rho, sd) ++ 
            codev(body, rho + (x -> (VarKind.Local,sd+1)), sd+1) :+ SLIDE(1)
          case _ => throw new Exception("TODO")
        }
        case LetRec(body,patDef@_*) => {
          val n = patDef.length
          val list = (1 to n).toList
          def varI(i:Int):String = patDef apply(i) _1 match { 
            case patterns.Id(x) => x
            case _ => throw new Exception("TODO")
          }
          def update(r:HashMap[String,(VarKind.Value,Int)],i:Int)
            :HashMap[String,(VarKind.Value,Int)] = r + (varI(i) -> (VarKind.Local,sd+i))
          val rhoNew = (list foldLeft rho)(update _)
          
          ALLOC(n) +:
            // TODO codev inside of flatMap to be replaced by codec when CBN is used
            (list.flatMap((i:Int)=>codev(patDef apply(i) _2, rhoNew, sd+n) :+ REWRITE(n-i+1)) ++
            codev(body,rhoNew,sd+n) :+ SLIDE(n))
        }
        case Lambda(body,args) => {
          List.empty  
        }
        case _ => throw new Exception("TODO")
      }
    
    def getvar(x:String, rho:HashMap[String,(VarKind.Value,Int)],sd:Int)
    :List[Instruction] = rho.get(x) match {
      case Some((VarKind.Global,i)) => List(PUSHGLOB(i))
        case Some((VarKind.Local,i)) => List(PUSHLOC(sd-i))
        case _ => throw new Exception("Undefined variable in codegen-phase")
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
			case UnaryOperator.neg => NEG
    }
    
    def newLabel():LABEL = {
      counter = counter + 1 
      LABEL("A"+counter)
    }
    
    def free(e:Expression,vs:Set[Id]):Set[Id] = e match {
      case x:Const => Set.empty
      case x@Id(_) => if (vs contains x) Set.empty else Set(x)
      case UnOp(_,e) => free(e,vs)
      case BinOp(_,e1,e2) => free(e1,vs) ++ free(e2,vs)
      case IfThenElse(c,e1,e2) => free(c,vs) ++ free(e1,vs) ++ free(e2,vs)
      case App(f,args@_*) => 
        args.toList.foldLeft(free(f,vs))((s:Set[Id],arg:Expression) => s ++ free(arg,vs))
      //case Lambda(body,params@_*) => free(body, vs ++ params.toSet)
    }
  }
}
