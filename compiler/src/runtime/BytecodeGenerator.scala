package runtime
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.Type._
import codegen.mama.mamaInstructions._

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
	
}