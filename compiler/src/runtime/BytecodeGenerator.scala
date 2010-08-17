package runtime
import scala.collection.immutable.HashMap
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, POP => MAMAPOP, _}
//import codegen.mama.{mamaInstructions => mama}

// TODO inherit from some better suited exception
class CompilerAssertion(msg: String) extends Exception

class BytecodeGenerator(mv: MethodVisitor, labels:HashMap[LABEL,Label], continueLabel:Label) {

	/* loads value 1 from the frame. this value holds a Machine instance */
	def aload = {
		mv.visitVarInsn(ALOAD, 1)
		this
	}

	/*
	 * there is a version of bipush, iconst_<i> that can push from (-1...5)
	 * but that's an optimization, we always use bipush for constants
	 */
	def bipush(constant: Int) = {
		mv.visitIntInsn(BIPUSH, constant)
		this
	}

	def sipush(constant: Int) = {
		mv.visitIntInsn(SIPUSH, constant)
		this
	}

	/* ldc can also support objects other than Int, but I don't casre in the
	 * slightest
	 */
	def ldc(constant: Int) = {
		mv.visitLdcInsn(constant)
		this
	}

	def iconst(variant: Int) = {
		mv.visitInsn(variant)
		this
	}

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
		mv.visitVarInsn(ISTORE, 2)
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

	def jumpto() = {
		// store the value that is on the stack in _goto
		mv.visitVarInsn(ISTORE, 2)
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
		jumpto

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
		case LOADC(constant) => aload pushInt constant invokevirtual("loadc", "(I)V")
		case MKBASIC => aload invokevirtual("mkbasic", "()V")
		case PUSHLOC(value) => aload pushInt value invokevirtual("pushloc", "(I)V")
		case GETBASIC => aload invokevirtual("getbasic", "()V")
		case MUL => aload invokevirtual("mul", "()V")
		case ADD => aload invokevirtual("add", "()V")
		case SLIDE(depth) => aload pushInt depth invokevirtual("slide", "(I)V")
		case SETLABEL(label) => enterLabel(label)
		case EQ => aload invokevirtual("eq", "()V")
		case JUMPZ(label) => aload invokevirtual("popraw", "()I") jumpz(label)
		case JUMP(label) => jump(label)
		case ALLOC(value) => aload pushInt value invokevirtual("alloc", "(I)V")
		case MKVEC(length) => aload pushInt length invokevirtual("mkvec", "(I)V")
		case MKCLOS(LABEL(l)) => aload pushInt l invokevirtual("mkclos", "(I)V")
		case UPDATE => aload invokevirtual("update", "()I") jumpto
		case PUSHGLOB(n) => aload pushInt n invokevirtual("pushglob", "(I)V")
		case EVAL(LABEL(l)) => aload pushInt l invokevirtual("eval", "(I)I") jumpto
		case MARK(LABEL(l)) => aload pushInt l invokevirtual("mark", "(I)V")
		case MKFUNVAL(LABEL(l)) => aload pushInt l invokevirtual("mkfunval", "(I)V")
		case TARG(drop, LABEL(l)) => aload pushInt drop pushInt l
			invokevirtual("targ", "(II)I") handleOptionalJump
		case MAMARETURN(n) => aload pushInt n invokevirtual("return_", "(I)I")
			handleOptionalJump
		case APPLY => aload invokevirtual("apply", "()I") jumpto
		case REWRITE(n) => aload pushInt n invokevirtual("rewrite", "(I)V")
		case MAMAPOP => aload invokevirtual("pop", "()V")
		case HALT => aload invokevirtual("halt", "()V")
		case GET(n) => aload pushInt n invokevirtual("get", "(I)V")
		case AND => aload invokevirtual("and", "()V")
		case SUB => aload invokevirtual("sub", "()V")
		case NIL => aload invokevirtual("nil", "()V")
		case CONS => aload invokevirtual("cons", "()V")
		case TLIST(LABEL(l)) => aload pushInt l invokevirtual("tlist", "(I)I")
			handleOptionalJump
		case LE => aload invokevirtual("le", "()V")
		case NOT => aload invokevirtual("not", "()V")
		case NEG => aload invokevirtual("neg", "()V")
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
		mv.visitVarInsn(ASTORE, 1)
		// store _goto = 0
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, 2)
		// store _terminate = 0
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, 3)

		val continueLabel = new Label()

		mv.visitLabel(continueLabel)
		mv.visitVarInsn(ILOAD, 3)
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

		//println((Array[Int](0) ++ compilerLabels.toArray[Int] ++ Array[Int](compilerLabels.last + 1)).toList)
		//println((Array[Label](switchEntry) ++ jvmLabels.toArray[Label] ++ Array[Label](doTerminateLabel)).toList)

		// generate a switch statement with defaultLabel as default case
		mv.visitLookupSwitchInsn(defaultLabel,
			Array[Int](0) ++ compilerLabels.toArray[Int] ++ Array[Int](compilerLabels.last + 1),
			Array[Label](switchEntry) ++ jvmLabels.toArray[Label] ++ Array[Label](doTerminateLabel))

		// entry case
		mv.visitLabel(switchEntry)

		// from here starts the actual code generation
		// we need to discover the labels first
		val gen = new BytecodeGenerator(mv, knownLabels, continueLabel)
		// generate the instructions
		instr map gen.generateInstruction

		// termination case
		mv.visitLabel(doTerminateLabel)
		mv.visitInsn(ICONST_1)
		mv.visitVarInsn(ISTORE, 3)

		// default case
		mv.visitLabel(defaultLabel)
		// TODO: remove this. This is only meant to determine whether we
		// ran into an invalid label
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "halt", "()V")
		// End of TODO
		mv.visitJumpInsn(GOTO, continueLabel)

		// outside of while loop
		mv.visitLabel(terminateCheckLabel)

		mv.visitVarInsn(ALOAD, 1)
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