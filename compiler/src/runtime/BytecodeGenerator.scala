package runtime
import java.io.{FileWriter, BufferedWriter}
import org.objectweb.asm._

object BytecodeGenerator {
  def generateMain(): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.toByteArray
  }
  def main(args: Array[String]) {
    val filename = args(1)
    val fileStream = new FileWriter(filename);
    val out = new BufferedWriter(fileStream);
    val bytecode = new String(generateMain())
    out.write(bytecode)
    out.close()
  }
}
