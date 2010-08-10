package runtime
import java.io.{FileWriter, BufferedWriter}
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
	def generateMain(): String = {
		val `class` = new ClassWriter(0)
		`class`.visit(V1_5, ACC_PUBLIC + ACC_STATIC, "Foo", null,
									"java/lang/Object", new Array[String](0))
		val main = `class`.visitMethod(ACC_PUBLIC + ACC_STATIC, "main",
																	 "([Ljava/lang/String;)V", null, null)
		main.visitCode()
		main.visitMaxs(1, 1)
		main.visitEnd()
		// build bytecodes for each instruction
		`class`.visitEnd()
		new String(`class`.toByteArray())
	}
	def main(args: Array[String]) {
		//val filename = args(1)
		val filename = "Foo.class"
		val fileStream = new FileWriter(filename);
		val out = new BufferedWriter(fileStream);
		out.write(generateMain())
		out.close()
	}
}