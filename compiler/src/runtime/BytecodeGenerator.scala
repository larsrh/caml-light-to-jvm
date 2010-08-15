package runtime
import scala.collection.immutable.HashMap
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, _}

// TODO inherit from some better suited exception
class CompilerAssertion(msg: String) extends Exception

class BytecodeGenerator(mv: MethodVisitor, labels:HashMap[LABEL,Label]) {

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
	// TODO: implement iconst_<i> ? Not much use probably

	def invokevirtual(name: String, sig: String) = {
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", name, sig)
		this
	}

	def condjump(comp: Int, label: LABEL) = {
		labels get label match {
			case Some(l) => mv.visitJumpInsn(comp, l)
			case None => throw new CompilerAssertion("jump target unknown")
		}
	}

	def enterLabel(label: LABEL) = {
		labels get label match {
			case Some(l) => mv.visitLabel(l)
			case None => throw new CompilerAssertion("trying to enter unknown label")
		}
		this
	}

	def generateInstruction(instr: Instruction) = { instr match {
			case LOADC(constant) => aload bipush constant invokevirtual("loadc", "(I)V")
			case MKBASIC => aload invokevirtual("mkbasic", "()V")
			case PUSHLOC(value) => aload bipush value invokevirtual("pushloc", "(I)V")
			case GETBASIC => aload invokevirtual("getbasic", "()V")
			case MUL => aload invokevirtual("mul", "()V")
			case ADD => aload invokevirtual("add", "()V")
			case SLIDE(depth) => aload bipush depth invokevirtual("slide", "(I)V")
			case SETLABEL(label) => enterLabel(label)
			case EQ => aload invokevirtual("eq", "()V")
			case JUMPZ(label) => aload invokevirtual("popraw", "()I") condjump(IFEQ, label)
			case JUMP(label) => condjump(GOTO, label)
			case ALLOC(value) => aload bipush value invokevirtual("alloc", "(I)V")
		}
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
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, 2)
		mv.visitInsn(ICONST_0)
		mv.visitVarInsn(ISTORE, 3)

		val l0 = new Label()

		mv.visitLabel(l0)
		mv.visitVarInsn(ILOAD, 3)
		val l1 = new Label()

		mv.visitJumpInsn(IFNE, l1)
		mv.visitVarInsn(ILOAD, 2)
		val l2 = new Label()

		val l3 = new Label()

		val l4 = new Label()

		val l5 = new Label()

		val l6 = new Label()

		mv.visitLookupSwitchInsn(l6, Array[Int](0, 1, 2, 3),
			Array[Label](l2, l3, l4, l5))

		mv.visitLabel(l2)
		mv.visitLabel(l3)
		mv.visitVarInsn(ALOAD, 1)
		mv.visitIntInsn(BIPUSH, 97)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitIntInsn(BIPUSH, 97)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "eq", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "popraw", "()I")
		val l7 = new Label()

		mv.visitJumpInsn(IFNE, l7)
		mv.visitInsn(ICONST_2)
		mv.visitVarInsn(ISTORE, 2)
		mv.visitJumpInsn(GOTO, l0)
		mv.visitLabel(l7)
		mv.visitVarInsn(ALOAD, 1)
		mv.visitIntInsn(BIPUSH, 97)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
		mv.visitInsn(ICONST_3)
		mv.visitVarInsn(ISTORE, 2)
		mv.visitJumpInsn(GOTO, l0)
		mv.visitLabel(l4)
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_0)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
		mv.visitLabel(l5)
		mv.visitInsn(ICONST_1)
		mv.visitVarInsn(ISTORE, 3)
		mv.visitLabel(l6)
		mv.visitJumpInsn(GOTO, l0)

		// from here starts the actual code generation
		// we need to discover the labels first
		//val gen = new BytecodeGenerator(mv, discoverLabels(instr))
		// generate the instructions
		//instr map gen.generateInstruction

		// end of generated code
		mv.visitLabel(l1);
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "_pstack", "()V")
		mv.visitInsn(RETURN)
		// (0, 0) might be a better idea
		mv.visitMaxs(2, 2)
		mv.visitEnd
	}
	override def visitEnd() = {
		// add our custom main method now
		injectMain

		// maybe the parent wants to do some fancy stuff
		super.visitEnd
	}
}