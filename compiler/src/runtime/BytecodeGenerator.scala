package runtime
import java.io.{FileWriter, BufferedWriter}
import org.objectweb.asm._
import org.objectweb.asm.Opcodes._

object BytecodeGenerator {
  def generateMain(): String = {
    val cw = new ClassWriter(0)
    cw.visit(V1_5, ACC_PUBLIC + ACC_STATIC, "Foo", null, "java/lang/Object", new Array[String](0))
    cw.visitEnd()
    new String(cw.toByteArray())
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
