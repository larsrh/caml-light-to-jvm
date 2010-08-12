package runtime
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, _}

object BytecodeGenerator {
	def loadc(constant: Int, mv: MethodVisitor) = {
		mv.visitVarInsn(ALOAD, 1)
		mv.visitIntInsn(BIPUSH, 19)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
	}

	def generateInstruction(instr: Instruction, mv: MethodVisitor) = { instr match {
			case LOADC(constant) => loadc(constant, mv)
		}
	}
}

class BytecodeAdapter(cv: ClassVisitor, instr: List[Instruction]) extends ClassAdapter(cv) {
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

		// from here starts the actual content
		for (single <- instr) {
			BytecodeGenerator.generateInstruction(single, mv)
		}

		//mv.visitVarInsn(ALOAD, 1)
		//mv.visitIntInsn(BIPUSH, 19)
		//mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
		// 
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "mkbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_0)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "pushloc", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "getbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "pushloc", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "getbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "mul", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "mkbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "pushloc", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "getbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "pushloc", "(I)V");
		mv.visitVarInsn(ALOAD, 1);
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "getbasic", "()V");
		mv.visitVarInsn(ALOAD, 1);
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "add", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "mkbasic", "()V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "slide", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitInsn(ICONST_1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "slide", "(I)V")
		mv.visitVarInsn(ALOAD, 1)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "getbasic", "()V")
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