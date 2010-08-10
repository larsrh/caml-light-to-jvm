package runtime
import java.io.FileOutputStream
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions._

object BytecodeGenerator {
	/*
	def generateInstruction(instr: Instruction, writer: ClassWriter) = { instr match {
			case ADD => // JVM bytecode call
		}
	}
	*/
	def generateClass(): Array[Byte] = {
		val `class` = new ClassWriter(ClassWriter.COMPUTE_MAXS +
																	ClassWriter.COMPUTE_FRAMES)
		`class`.visit(V1_5, ACC_PUBLIC + ACC_SUPER, "Foo", null,
									"java/lang/Object", null)

		// does not seem to be neccessary, yet the java compiler generates this
		/*
		val init = `class`.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
		init.visitCode()
		init.visitVarInsn(ALOAD, 0)
		init.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V")
		init.visitInsn(RETURN)
		init.visitMaxs(1, 1)
		init.visitEnd()
		*/

		val main = `class`.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
																	 "([Ljava/lang/String;)V", null, null)
		main.visitCode()
		main.visitFieldInsn(GETSTATIC, "java/lang/System", "out",
												"Ljava/io/PrintStream;")
		main.visitLdcInsn("Hello World")
		main.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println",
												 "(Ljava/lang/String;)V")
		main.visitInsn(Opcodes.RETURN)
		main.visitMaxs(2, 1)
		main.visitEnd()

		// build bytecodes for each instruction

		`class`.visitEnd()
		`class`.toByteArray()
	}
	def main(args: Array[String]) {
		//val filename = args(1)
		val filename = "Foo.class"
		val fileStream = new FileOutputStream(filename);
		fileStream.write(generateClass())
		fileStream.close()
	}
}