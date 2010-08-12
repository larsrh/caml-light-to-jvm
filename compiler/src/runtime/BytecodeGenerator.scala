package runtime
import scala.collection.immutable.HashMap
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, _}

// TODO inherit from some better suited exception
class CompilerAssertion(msg: String) extends Exception

class BytecodeGenerator(mv: MethodVisitor, labels:HashMap[LABEL,Label]) {

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

	def enterLabel(label: LABEL) = {
		labels get label match {
			case Some(l) => mv.visitLabel(l)
			case None => throw new CompilerAssertion("trying to enter unknown label")
		}
		this
	}

	def generateInstruction(instr: Instruction) = { instr match {
			case LOADC(constant) => aload bipush 19 invokevirtual("loadc", "(I)V")
			case MKBASIC => aload invokevirtual("mkbasic", "()V")
			case PUSHLOC(value) => aload bipush value invokevirtual("pushloc", "(I)V")
			case GETBASIC => aload invokevirtual("getbasic", "()V")
			case MUL => aload invokevirtual("mul", "()V")
			case ADD => aload invokevirtual("add", "()V")
			case SLIDE(depth) => aload bipush depth invokevirtual("slide", "(I)V")
			case SETLABEL(label) => enterLabel(label)
		}
	}
}

/*
 * used to inject our own main() method, which contains our compiled code
 */
class BytecodeAdapter(cv: ClassVisitor, instr: List[Instruction]) extends ClassAdapter(cv) {
	def discoverLabels(instr: List[Instruction]) = {
		// get all SETLABELs
		val setLabels = instr.filter({case SETLABEL(l) => true case _ => false})
		// unwrap the LABELs
		val mamaLabels = setLabels.map({case SETLABEL(l) => l})
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
		// TODO: add code for while loop + switch case so we can jump to labels

		// from here starts the actual code generation
		// we need to discover the labels first
		val gen = new BytecodeGenerator(mv, discoverLabels(instr))
		// generate the instructions
		instr map gen.generateInstruction

		// end of generated code
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