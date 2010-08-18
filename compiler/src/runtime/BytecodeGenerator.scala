package runtime
import scala.collection.immutable.HashMap
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, POP => MAMAPOP, _}
//import codegen.mama.{mamaInstructions => mama}

// TODO inherit from some better suited exception
class CompilerAssertion(msg: String) extends Exception

class BytecodeGenerator(mv: MethodVisitor, labels: HashMap[LABEL,Label],
	continueLabel: Label, machineLocation: Int, gotoLocation: Int) {

	/* loads the machine instance from the frame */
	def pushMachine = {
		mv.visitVarInsn(ALOAD, machineLocation)
		this
	}

	/* iconst can push -1 to 5 (constant) onto the JVM operand stack */
	def iconst(variant: Int) = {
		mv.visitInsn(variant)
		this
	}

	/*
	 * bipush pushes smaller numbers onto the stack, beware: it wraps around if
	 * you pass too large numbers without warning.
	 */
	def bipush(constant: Int) = {
		mv.visitIntInsn(BIPUSH, constant)
		this
	}

	/* sipush pushes slightly bigger numbers onto the JVM operand stack */
	def sipush(constant: Int) = {
		mv.visitIntInsn(SIPUSH, constant)
		this
	}

	/*
	 * ldc can also support objects other than Int, but I don't casre in the
	 * slightest
	 */
	def ldc(constant: Int) = {
		mv.visitLdcInsn(constant)
		this
	}

	/*
	 * this method is used to push ANY number onto the JVM operand stack.
	 * it dispatches depending on the number size to the appropriate JVM
	 * instruction calls.
	 */
	def pushInt(constant: Int) = {
		constant match {
			case -1 => iconst(ICONST_M1)
			case 0 => iconst(ICONST_0)
			case 1 => iconst(ICONST_1)
			case 2 => iconst(ICONST_2)
			case 3 => iconst(ICONST_3)
			case 4 => iconst(ICONST_4)
			case 5 => iconst(ICONST_5)
			case n if n >= -128 && n < 127 => bipush(n)
			case n if n >= -32768 && n < 32767 => sipush(n)
			case n => ldc(n)
		}
		this
	}

	def storeGoto(constant: Int) = {
		pushInt(constant)
		mv.visitVarInsn(ISTORE, gotoLocation)
		this
	}

	def jumpGoto() = {
		mv.visitJumpInsn(GOTO, continueLabel)
		this
	}

	// TODO: implement iconst_<i> ? Not much use probably

	def invokevirtual(name: String, sig: String) = {
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", name, sig)
		this
	}

	def jump(label: LABEL) = {
		storeGoto(label.nr) jumpGoto
	}

	def jumpz(label: LABEL) = {
		val nonZero = new Label()

		mv.visitJumpInsn(IFNE, nonZero)
		// zero
		jump(label)

		mv.visitLabel(nonZero)
		// nonzero
		this
	}

	def jumpTo() = {
		// store the value that is on the stack in _goto
		mv.visitVarInsn(ISTORE, gotoLocation)
		// and jump
		jumpGoto
		this
	}

	/*
	 * eval either returns a label (we have to jump to that) or -1, which
	 * we have to ignore. handleEval generates the code that compares the result
	 * to -1 and jumps
	 */
	def handleOptionalJump() = {
		val noJump = new Label()

		// duplicate the return value of eval
		mv.visitInsn(DUP)
		// push -1 to compare it with
		pushInt(-1)
		// if it was equal to -1, jump to noAct
		mv.visitJumpInsn(IF_ICMPEQ, noJump)
		// otherwise, jump to that value
		jumpTo

		// eval return value was -1
		mv.visitLabel(noJump)
		// just pop the return value
		mv.visitInsn(POP)
		this
	}
	
	def enterLabel(label: LABEL) = {
		labels get label match {
			case Some(l) => mv.visitLabel(l)
			case None => throw new CompilerAssertion("trying to enter unknown label")
		}
		this
	}

	def generateInstruction:Instruction=>Any = {
		case LOADC(constant) => pushMachine pushInt constant invokevirtual("loadc", "(I)V")
		case MKBASIC => pushMachine invokevirtual("mkbasic", "()V")
		case PUSHLOC(value) => pushMachine pushInt value invokevirtual("pushloc", "(I)V")
		case GETBASIC => pushMachine invokevirtual("getbasic", "()V")
		case MUL => pushMachine invokevirtual("mul", "()V")
		case ADD => pushMachine invokevirtual("add", "()V")
		case SLIDE(depth) => pushMachine pushInt depth invokevirtual("slide", "(I)V")
		case SETLABEL(label) => enterLabel(label)
		case EQ => pushMachine invokevirtual("eq", "()V")
		case JUMPZ(label) => pushMachine invokevirtual("popraw", "()I") jumpz(label)
		case JUMP(label) => jump(label)
		case ALLOC(value) => pushMachine pushInt value invokevirtual("alloc", "(I)V")
		case MKVEC(length) => pushMachine pushInt length invokevirtual("mkvec", "(I)V")
		case MKCLOS(LABEL(l)) => pushMachine pushInt l invokevirtual("mkclos", "(I)V")
		case UPDATE => pushMachine invokevirtual("update", "()I") jumpTo
		case PUSHGLOB(n) => pushMachine pushInt n invokevirtual("pushglob", "(I)V")
		case EVAL(LABEL(l)) => pushMachine pushInt l invokevirtual("eval", "(I)I") jumpTo
		case MARK(LABEL(l)) => pushMachine pushInt l invokevirtual("mark", "(I)V")
		case MKFUNVAL(LABEL(l)) => pushMachine pushInt l invokevirtual("mkfunval", "(I)V")
		case TARG(drop, LABEL(l)) => pushMachine pushInt drop pushInt l
			invokevirtual("targ", "(II)I") handleOptionalJump
		case MAMARETURN(n) => pushMachine pushInt n invokevirtual("return_", "(I)I")
			handleOptionalJump
		case APPLY => pushMachine invokevirtual("apply", "()I") jumpTo
		case REWRITE(n) => pushMachine pushInt n invokevirtual("rewrite", "(I)V")
		case MAMAPOP => pushMachine invokevirtual("pop", "()V")
		case HALT => pushMachine invokevirtual("halt", "()V")
		case GET(n) => pushMachine pushInt n invokevirtual("get", "(I)V")
		case AND => pushMachine invokevirtual("and", "()V")
		case SUB => pushMachine invokevirtual("sub", "()V")
		case NIL => pushMachine invokevirtual("nil", "()V")
		case CONS => pushMachine invokevirtual("cons", "()V")
		case TLIST(LABEL(l)) => pushMachine pushInt l invokevirtual("tlist", "(I)I")
			handleOptionalJump
		case LE => pushMachine invokevirtual("le", "()V")
		case NOT => pushMachine invokevirtual("not", "()V")
		case NEG => pushMachine invokevirtual("neg", "()V")
		case COPYGLOB => pushMachine invokevirtual("copyglob", "()V")
		case DIV => pushMachine invokevirtual("div", "()V")
		case GEQ => pushMachine invokevirtual("geq", "()V")
		case GETVEC(k) => pushMachine pushInt k invokevirtual("getvec", "(I)V")
		case GR => pushMachine invokevirtual("gr", "()V")
		case LABEL(name) => // LABELs don't get compiled to anything
		case LEQ => pushMachine invokevirtual("leq", "()V")
		case NEQ => pushMachine invokevirtual("neq", "()V")
		case OR => pushMachine invokevirtual("or", "()V")
		case POPENV => pushMachine invokevirtual("popenv", "()V") jumpTo
		case WRAP(LABEL(l)) => pushMachine pushInt l invokevirtual("wrap", "(I)V")
	}
}

/*
 * used to inject our own main() method, which contains our compiled code
 */
class BytecodeAdapter(cv: ClassVisitor, instr: List[Instruction]) extends ClassAdapter(cv) {
	def discoverLabels(instr: List[Instruction]) = {
		// extract all LABELs from all SETLABELs
		val mamaLabels = instr collect {case SETLABEL(l) => l}
		// construct a hash containing the labels
		mamaLabels.foldLeft(HashMap.empty[LABEL,Label])((h, e) => h + (e -> new Label()))
	}

	def injectMain() = {
		// method definition
		val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
														"([Ljava/lang/String;)V", null, null)
		// code: instantiate Machine and store it in position 1
		mv.visitCode
		mv.visitTypeInsn(NEW, "runtime/Machine")
		mv.visitInsn(DUP)
		mv.visitMethodInsn(INVOKESPECIAL, "runtime/Machine", "<init>", "()V")
		val machineLocation = 1
		mv.visitVarInsn(ASTORE, machineLocation)
		// store _goto = 0
		val gotoLocation = 2
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, gotoLocation)
		// store _terminate = 0
		val terminateLocation = 3
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, terminateLocation)

		val continueLabel = new Label()

		mv.visitLabel(continueLabel)
		mv.visitVarInsn(ILOAD, terminateLocation)
		val terminateCheckLabel = new Label()

		mv.visitJumpInsn(IFNE, terminateCheckLabel)
		mv.visitVarInsn(ILOAD, 2)

		val switchEntry = new Label()

		val knownLabels = discoverLabels(instr)

		val doTerminateLabel = new Label()

		val defaultLabel = new Label()

		val orderedLabels = knownLabels.toList.sortBy(_._1)
		val compilerLabels = orderedLabels.collect({case a => a._1.nr})
		val jvmLabels = orderedLabels.collect({case a => a._2})

		// generate a switch statement with defaultLabel as default case
		mv.visitLookupSwitchInsn(defaultLabel,
			Array[Int](0) ++ compilerLabels.toArray[Int] ++ Array[Int](compilerLabels.last + 1),
			Array[Label](switchEntry) ++ jvmLabels.toArray[Label] ++ Array[Label](doTerminateLabel))

		// entry case
		mv.visitLabel(switchEntry)

		// from here starts the actual code generation
		// we need to discover the labels first
		val gen = new BytecodeGenerator(mv, knownLabels, continueLabel,
			machineLocation, gotoLocation)
		// generate the instructions
		instr map gen.generateInstruction

		// termination case
		mv.visitLabel(doTerminateLabel)
		mv.visitInsn(ICONST_1)
		mv.visitVarInsn(ISTORE, terminateLocation)

		// default case
		mv.visitLabel(defaultLabel)
		// don't do anything in the default label, just jump to the loop again
		mv.visitJumpInsn(GOTO, continueLabel)

		// outside of while loop
		mv.visitLabel(terminateCheckLabel)

		mv.visitVarInsn(ALOAD, machineLocation)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "_pstack", "()V")
		mv.visitInsn(RETURN)
		// (0, 0) might be a better idea
		mv.visitMaxs(2, 4)
		mv.visitEnd
	}
	override def visitEnd() = {
		// add our custom main method now
		injectMain

		// maybe the parent wants to do some fancy stuff
		super.visitEnd
	}
}