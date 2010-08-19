package entry

import scala.io.Source
import scala.collection.immutable.HashMap

import java.io.FileNotFoundException

import org.github.scopt._

import parser._
import codegen.mama.Translator
import typeinference.TypeInference
import codegen.mama.mamaInstructions.Instruction
import runtime.Assembly

abstract class Key {
	type Value
}

object Key {
	def apply[V]() = new Key { type Value = V }
}

class TypedMap[-KeyType <: Key] {
	private[this] val backed = new collection.mutable.HashMap[Any, Any]()

	def get[K <: KeyType](key: K) = backed.get(key).map(_.asInstanceOf[K#Value])
	def apply[K <: KeyType](key: K) = backed(key).asInstanceOf[K#Value]
	def update[K <: KeyType, V <: K#Value](key: K, value: V) { backed(key) = value }

	override def toString = backed.toString
}


object Entrypoint {

	def genByteCode(l: List[Instruction], filename:String) {
		val jar = new Assembly(filename)
		jar.addManifest
		jar.copyClasses
		jar.injectCode(l)
		jar.close
	}

	def error(msg: String) {
		System.err.println(msg)
	}

	def output(l: List[Instruction], filename: String) {
			val out = new java.io.FileWriter(filename)
			l map ((i: Instruction) => out write (i.toString))
			out close
	}

	def main(args: Array[String]) {
		val config = new TypedMap[Key]
		val outputFile = Key[String]
		val inputFile = Key[String]
		val generateMama = Key[Boolean]

		val parser = new OptionParser("compiler") {
			opt("o", "output", "the output file, default to a.jar",
				{v: String => config(outputFile) = v})
			booleanOpt("m", "mama", "generate MaMa code instead of bytecode",
				{v: Boolean => config(generateMama) = v})
			arg("file", "CamlLight source code",
				{v: String => config(inputFile) = v})
		}
		if (parser.parse(args)) {
			val fileName = (config.get(outputFile), config.get(generateMama)) match {
				case (Some(filename), Some(true)) => filename + ".mama"
				case (Some(filename), x) => filename + ".jar"
				case (None, Some(true)) => "a.mama"
				case (None, x) => "a.jar"
			}

			try {
				config get inputFile orElse {
					error("Input file not given")
					None
				} flatMap { camlSourceFile =>
					try {
						Some(Source.fromFile(camlSourceFile).mkString)
					}
					catch {
						case ex: FileNotFoundException =>
							error("Input file not found")
							None
					}
				} flatMap { content =>
					try {
						Some(Parser.parse(content))
					}
					catch {
						case ex: ScannerException =>
							error("Scanner exception occured")
							error(ex.toString)
							None
						case ex: ParserException =>
							error("Parse exception occured")
							error(ex.toString)
							None
					}
				} flatMap { prog =>
					{
						try {
							Some(TypeInference.typeCheck(TypeInference.emptyEnv, prog.expr) _2)
						}
						catch {
							case TypeInference.UnificationError(msg, expr) =>
								error("Type checking failed")
								error(msg)
								error(expr.prettyPrint)
								prog.positions get expr foreach { p => error(p.toString) }
								None
							case TypeInference.TypeError(msg) =>
								error("Type checking failed")
								error(msg)
								None
						}
					} flatMap { gamma =>
						try {
							Some((new Translator(prog.positions,gamma)).codeb(prog.expr, HashMap.empty, 0))
						}
						catch {
							case ex: Exception =>
								error("Translation failed")
								error(ex.toString)
								None
						}
					}
				} foreach { case mama =>
					config.get(generateMama) match {
						case Some(true) => output(mama, fileName)
						case x => genByteCode(mama, fileName)
					}
				}
			}
			catch {
				case ex: Exception =>
					error("Unknown exception occured")
					ex.printStackTrace()
			}
		}
	}
}
