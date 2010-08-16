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
		val cr = new ClassReader(bytesInput)
		val cw = new ClassWriter(cr, ClassWriter.COMPUTE_MAXS +
														 ClassWriter.COMPUTE_FRAMES)

		/* begin TODO: this is currently hardcoded */
		/*
		val instr = List(LOADC(19), MKBASIC, PUSHLOC(0), GETBASIC, PUSHLOC(1),
			GETBASIC, MUL, MKBASIC, PUSHLOC(1), GETBASIC, PUSHLOC(1), GETBASIC,
			ADD, MKBASIC, SLIDE(1), SLIDE(1), GETBASIC)
		*/

		/*
		// e8
		val l1 = LABEL(1)
		val l2 = LABEL(2)
		val instr = List(LOADC(97), LOADC(97), EQ, JUMPZ(l1), LOADC(42),
			JUMP(l2), SETLABEL(l1), LOADC(0), SETLABEL(l2))
		*/

		import parser.ast.expressions._
		import parser.ast.patterns
		import codegen.mama.Translator
		import scala.collection.immutable.HashMap

		// let a = 19 in let b = a * a in a + b
		// EXPECTED RESULT 380
		val e0 = Let(patterns.Id("a"),Integer(19),
								 Let(patterns.Id("b"),BinOp(BinaryOperator.mul,Id("a"),Id("a")),
										 BinOp(BinaryOperator.add,Id("a"),Id("b"))
			)
		)

		// (\a.a) 42
		// EXPECTED RESULT 42
		val e1 = App(Lambda(Id("a"),patterns.Id("a")),Integer(42))

		// (\a.a + 3) 42
		// EXPECTED RESULT 45
		val e2 = App(Lambda(BinOp(BinaryOperator.add,Id("a"),Integer(3)),patterns.Id("a")),Integer(42))

		// let a = \a.a in a a
		// EXPECTED RESULT NOT BASIC ERROR
		val e3 = Let(patterns.Id("a"), Lambda(Id("a"), patterns.Id("a")),
								 App(Id("a"),Id("a")))

		// (let a = \a.a in a a) 42
		// EXPECTED RESULT 42
		val e4 = App(e3,Integer(42))

		// \a.(\b.(\c.(a b) (b c)))
		// EXPECTED RESULT NOT BASIC ERROR
		val e5 = Lambda(Lambda(Lambda(
					App(App(Id("a"),Id("b")), App(Id("b"), Id("c"))), patterns.Id("c")), patterns.Id("b")), patterns.Id("a"))

		// let a = \b.b in let b = \a.a in \a.a
		// EXPECTED RESULT NOT BASIC ERROR
		val e6 = Let(patterns.Id("a"),
								 Lambda(Id("b"), patterns.Id("b")),
								 Let(patterns.Id("b"),
										 App(Id("a"), Id("a")),
										 App(Id("a"), Id("a"))))

		// (let a = \b.b in let b = \a.a in \a.a) 'a'
		// EXPECTED RESULT 97
		val e7 = App(e6,Character('a'));

		// if 97 = 'a' then 42 else false
		// EXPECTED RESULT 42
		val e8 = IfThenElse(BinOp(BinaryOperator.eq,Integer(97),Character('a')),Integer(42),Bool(false))

		// match 2 with 3 -> true | 2 -> false
		// EXPECTED RESULT 0
		val e9 = Match(Integer(2),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))

		// match 4 with 3 -> true | 2 -> false
		// EXPECTED RESULT SOME ERROR
		val e10 = Match(Integer(4),(patterns.Integer(3),Bool(true)),(patterns.Integer(2),Bool(false)))

		// match 42 with x -> x
		// EXPECTED RESULT 42
		val e11 = Match(Integer(42),(patterns.Id("x"),Id("x")))

		val instr = Translator.codeb(e11, HashMap.empty, 0)

		/*
		val l118 = LABEL(118)
		val l119 = LABEL(119)
		val l120 = LABEL(120)
		val instr = List(ALLOC(0), LOADC(3), LOADC(2), EQ, JUMPZ(l119), LOADC(1),
			MKBASIC, SLIDE(0), JUMP(l118), SETLABEL(l119), ALLOC(0), LOADC(2),
			LOADC(2), EQ, JUMPZ(l120), LOADC(0), MKBASIC, SLIDE(0), JUMP(l118),
			SETLABEL(l120), SETLABEL(l118), GETBASIC)
		*/

		/* end of TODO */

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