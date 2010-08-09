
package codegen.mama

package mamaInstructions {
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
}

