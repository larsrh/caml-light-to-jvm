package runtime
import scala.collection.mutable.ListBuffer
import java.io.FileOutputStream
import java.io.InputStream
import java.util.zip.{ZipOutputStream, ZipEntry}
import org.objectweb.asm.{ClassWriter, ClassReader}
import codegen.mama.mamaInstructions._

object JarPacker {
	def createJar(fileName: String) = new ZipOutputStream(
		new FileOutputStream(fileName))

	def addManifest(zip: ZipOutputStream) = {
		zip.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))

		zip.write("""Manifest-Verion: 1.0
		Main-Class: runtime.Machine
		""" split "\n" map(_.trim) mkString "\n" getBytes)
		// the manifest needs to have a trailing newline
	}

	/* copies all Machine$* stuff that is not to be changed into the JAR */
	def copyClasses(zip: ZipOutputStream) = {
		val classes = List("1", // this is an anonymous class that we need to add
								 "Base", "Closure", "Function", "MachineData", "Raw", "Vector")

		for (entry <- classes) {
			val `class` = Class.forName("runtime.Machine$" + entry)
			val className = `class`.getName.replace(".", "/") + ".class"
			val bytecodeStream = `class`.getClassLoader.getResourceAsStream(className)

			val buf = new Array[Byte](1024);
			zip.putNextEntry(new ZipEntry("runtime/Machine$" + entry + ".class"))

			// push the whole stream into the file
			zip.write(BytesUtil.readWholeStream(bytecodeStream));
		}
	}

	/* writes Machine.class */
	def injectCode(zip: ZipOutputStream) = {
		import runtime.Machine

		val `class` = classOf[Machine]
		val className = `class`.getName.replace(".", "/") + ".class"
		val bytecodeStream = `class`.getClassLoader.getResourceAsStream(className)
		val bytesInput = BytesUtil.readWholeStream(bytecodeStream)

		// don't ask why ClassReader cannot load the class without failing
		//val cr = new ClassReader("runtime.Machine")
		val cr = new ClassReader(bytesInput)
		val cw = new ClassWriter(cr, ClassWriter.COMPUTE_MAXS +
														 ClassWriter.COMPUTE_FRAMES)
		// TODO: this is currently hardcoded and contains only one instruction
		val instr = List(LOADC(19), MKBASIC, PUSHLOC(0), GETBASIC, PUSHLOC(1),
			GETBASIC, MUL, MKBASIC, PUSHLOC(1), GETBASIC, PUSHLOC(1), GETBASIC,
			ADD, MKBASIC, SLIDE(1), SLIDE(1), GETBASIC)

		val ca = new BytecodeAdapter(cw, instr)
		cr.accept(ca, 0)

		zip.putNextEntry(new ZipEntry("runtime/Machine.class"))
		val bytes = cw.toByteArray
		zip.write(bytes, 0, bytes.length)
	}
  
  def main(args: Array[String]): Unit = {
    val filename = args(0)
		val jar = createJar(filename)
		addManifest(jar)
		copyClasses(jar)
		injectCode(jar)
		jar close
  }

}

object BytesUtil {
	/* returns the whole contents of the input stream into a byte array
	 * Was it so hard, Sun? I mean Oracle. */
	def readWholeStream(stream: InputStream): Array[Byte] = {
		val bufs = ListBuffer[Byte]()

		var done = false
		while (!done) {
			val char = stream.read()
			if (char == -1) {
				done = true
			} else {
				bufs += char.asInstanceOf[Byte]
			}
		}

		bufs.toArray[Byte]
	}
}