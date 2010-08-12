package runtime
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions.{RETURN => MAMARETURN, _}

object BytecodeGenerator {
	def generateInstruction(instr: Instruction, w: ClassWriter) = { instr match {
			case ADD => // JVM bytecode call
		}
	}

  def generateMain(instructions: List[Instruction], w: ClassWriter) = {
		for (instr <- instructions) {
			generateInstruction(instr, w)
		}
	}
}

class BytecodeAdapter(cv: ClassVisitor) extends ClassAdapter(cv) {
	override def visitEnd() = {
		// add our custom main method here
		val mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
														"([Ljava/lang/String;)V", null, null)
		mv.visitCode
		mv.visitTypeInsn(NEW, "runtime/Machine")
		mv.visitInsn(DUP)
		mv.visitMethodInsn(INVOKESPECIAL, "runtime/Machine", "<init>", "()V")
		mv.visitVarInsn(ASTORE, 1)
		// from here starts the actual content
		mv.visitVarInsn(ALOAD, 1)
		mv.visitIntInsn(BIPUSH, 19)
		mv.visitMethodInsn(INVOKEVIRTUAL, "runtime/Machine", "loadc", "(I)V")
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

		// maybe the parent wants to do some fancy stuff
		super.visitEnd
	}
}