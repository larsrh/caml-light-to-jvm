package runtime
import scala.collection.mutable.ListBuffer
import java.io.FileOutputStream
import java.io.InputStream
import java.util.zip.{ZipOutputStream, ZipEntry}
import org.objectweb.asm.{ClassWriter, ClassReader}
import codegen.mama.mamaInstructions._

class Assembly(filename: String) {
	val zip = new ZipOutputStream(new FileOutputStream(filename))

	def addManifest = {
		zip.putNextEntry(new ZipEntry("META-INF/MANIFEST.MF"))

		zip.write("""Manifest-Verion: 1.0
		Main-Class: runtime.Machine
		""" split "\n" map(_.trim) mkString "\n" getBytes)
		// the manifest needs to have a trailing newline
	}

	def copyClasses = {
		val classes = List("1", // this is an anonymous class that we need to add
								 "Base", "Closure", "Function", "List",  "Raw", "Vector", "Ref",
                 "StackData", "HeapData")

		for (entry <- classes) {
			val `class` = Class.forName("runtime.Machine$" + entry)
			val className = `class`.getName.replace(".", "/") + ".class"
			val bytecodeStream = `class`.getClassLoader.getResourceAsStream(className)

			val buf = new Array[Byte](1024)
			zip.putNextEntry(new ZipEntry("runtime/Machine$" + entry + ".class"))

			// push the whole stream into the file
			zip.write(BytesUtil.readWholeStream(bytecodeStream))
		}
	}

	def injectCode(instr:List[Instruction]) = {
		import runtime.Machine

		val `class` = classOf[Machine]
		val className = `class`.getName.replace(".", "/") + ".class"
		val bytecodeStream = `class`.getClassLoader.getResourceAsStream(className)
		val bytesInput = BytesUtil.readWholeStream(bytecodeStream)

		val cr = new ClassReader(bytesInput)
		val cw = new ClassWriter(cr, ClassWriter.COMPUTE_MAXS +
														 ClassWriter.COMPUTE_FRAMES)

		val ca = new BytecodeAdapter(cw, instr)
		cr.accept(ca, 0)

		zip.putNextEntry(new ZipEntry("runtime/Machine.class"))
		val bytes = cw.toByteArray
		zip.write(bytes, 0, bytes.length)
	}

	def close = zip.close
}

object JarPacker {
  def main(args: Array[String]): Unit = {
    val filename = args(0)
		/*
		val jar = createJar(filename)
		addManifest(jar)
		copyClasses(jar)

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

		// match (1,2,3) with (x,_,y) -> x + y
		// EXPECTED RESULT 4
		val e12 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
										(patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
										 BinOp(BinaryOperator.add,Id("x"),Id("y")))
		)

		// match (1,(2,3),(5,3)) with (x,_,y) -> match y with (2,_) -> x | (z,3) -> x + z + 36
		// EXPECTED RESULT 42
		val e13 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(3)),Tuple(Integer(5),Integer(3))),
										(patterns.Tuple(patterns.Id("x"),patterns.Underscore,patterns.Id("y")),
										 Match(Id("y"),
													 (patterns.Tuple(patterns.Integer(2),patterns.Underscore),Id("x")),
													 (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
														BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36))))))
		)

		// match (1,(2,4),(5,3)) with (x,z,y) -> match z with (2,x) -> x | (z,3) -> x + z + 36
		// EXPECTED RESULT 4
		val e14 = Match(Tuple(Integer(1),Tuple(Integer(2),Integer(4)),Tuple(Integer(5),Integer(3))),
										(patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Id("y")),
										 Match(Id("z"),
													 (patterns.Tuple(patterns.Integer(2),patterns.Id("x")),Id("x")),
													 (patterns.Tuple(patterns.Id("z"),patterns.Integer(3)),
														BinOp(BinaryOperator.add,Id("x"),BinOp(BinaryOperator.add,Id("z"),Integer(36))))))
		)

		// match (1,2,3) with (x,z,6) -> 5 | (1,2,y) -> y
		// EXPECTED RESULT 3
		val e15 = Match(Tuple(Integer(1),Integer(2),Integer(3)),
										(patterns.Tuple(patterns.Id("x"),patterns.Id("z"),patterns.Integer(6)),
										 Integer(5)),
										(patterns.Tuple(patterns.Integer(1),patterns.Integer(2),patterns.Id("y")),
										 Id("y"))
		)

		val tmp = LetRec(App(Id("fac"),Integer(5)),(patterns.Id("fac"),Lambda(Match(Id("00"),
			(patterns.Id("n"),IfThenElse(BinOp(BinaryOperator.eq,Id("n"),Integer(0)),Integer(1),BinOp(BinaryOperator.mul,Id("n"),App(Id("fac"),
			BinOp(BinaryOperator.sub,Id("n"),Integer(1))))))
			),
			patterns.Id("00"))))

		val instr = Translator.codeb(e15, HashMap.empty, 0)

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
		injectCode(jar, instr)
		jar close
		*/
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