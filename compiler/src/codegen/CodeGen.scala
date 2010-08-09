
package codegen.mama

package mamaInstructions {
  sealed class Instruction
  final case class LABEL(name:String) extends Instruction
  
  case object ADD extends Instruction
  case object AND extends Instruction
  final case class ALLOC(value:Int) extends Instruction
  case object CALL extends Instruction
  case object DIV extends Instruction
  case object DUP extends Instruction
  final case class ENTER(value:Int) extends Instruction
  case object EQ extends Instruction
  case object GEQ extends Instruction
  case object GR extends Instruction
  case object HALT extends Instruction
  final case class JUMP(target:LABEL) extends Instruction
  final case class JUMPZ(target:LABEL) extends Instruction
  final case class JUMPI(target:LABEL) extends Instruction
  case object LE extends Instruction
  case object LEQ extends Instruction
  case object LOAD extends Instruction
  final case class LOADA(address:Int) extends Instruction
  final case class LOADC(value:Int) extends Instruction
  final case class LOADR(address:Int, value:Int) extends Instruction
  final case class LOADRC(value:Int) extends Instruction
  final case class MALLOC(value:Int) extends Instruction
  case object MUL extends Instruction
  case object NEG extends Instruction
  case object NEQ extends Instruction
  case object NEW extends Instruction
  case object OR extends Instruction  
  case object POP extends Instruction  
  case object STORE extends Instruction
  final case class STOREA(address:Int) extends Instruction
  final case class STORER(address:Int, value:Int) extends Instruction
  case object SUB extends Instruction

  case object APPLY extends Instruction
  case object APPLY0 extends Instruction
  case object CONS extends Instruction
  case object COPYGLOB extends Instruction
  //case object EVAL extends Instruction // TODO macro
  case object GETBASIC extends Instruction
  final case class GETVEC(locvars:Int) extends Instruction
  final case class MARK(ret:LABEL) extends Instruction
  case object MARK0 extends Instruction
  case object MKBASIC extends Instruction
  final case class MKCLOS(target:LABEL) extends Instruction
  final case class MKFUNVAL(target:LABEL) extends Instruction
  final case class MKVEC(length:Int) extends Instruction
  case object MKVEC0 extends Instruction
  case object NIL extends Instruction
  final case class PUSHGLOB(value:Int) extends Instruction
  final case class PUSHLOC(value:Int) extends Instruction 
  // final case class RETURN(drop:Int) extends Instruction // TODO macro
  final case class REWRITE(depth:Int) extends Instruction 
  case object POPENV extends Instruction
  final case class SLIDE(drop:Int) extends Instruction
  // final case class TARG(drop:Int) extends Instruction // TODO macro
  case object UPDATE extends Instruction
  case object WRAP extends Instruction 
}

