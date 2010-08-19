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

	def get[K <: KeyType](key: K): Option[K#Value] = backed.get(key).map(_.asInstanceOf[K#Value])
	def apply[K <: KeyType](key: K): K#Value = backed(key).asInstanceOf[K#Value]
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

	private final class Chain[T](func: => Option[T]) {
		lazy val result =
			try {
				func
			}
			catch {
				case ex: Exception =>
					error("Unknown exception occured")
					ex.printStackTrace()
					None
			}

		def andThen[S](f: T => Option[S]): Chain[S] = result match {
			case Some(r) => new Chain(f(r))
			case None => new Chain(None)
		}

		def andThenChain[S](f: T => Chain[S]): Chain[S] = result match {
			case Some(r) => f(r)
			case None => new Chain(None)
		}

		def andFinally(f: T => Unit) { result match {
				case Some(r) => f(r)
				case None =>
			}
		}
	}
	
	private def chain[T](func: => Option[T]) = new Chain(func)

	def main(args: Array[String]) {
		val config = new TypedMap[Key]
		val outputFile = Key[String]
		val inputFile = Key[String]

		val parser = new OptionParser("compiler") {
			opt("o", "output", "the output file, default to a.jar",
				{v: String => config(outputFile) = v})
			arg("file", "CamlLight source code",
				{v: String => config(inputFile) = v})
		}
		if (parser.parse(args)) {
			val jarFile = config.get(outputFile) match {
				case Some(prefix) => prefix + ".jar"
				case None => "a.jar"
			}

			val camlSourceFile = config.get(inputFile) match {
				case Some(filename) => filename
				case None => {
						// as if that would ever happen...
						error("Input file not given")
						return
				}
			}

			chain {
				try {
					Some(Source.fromFile(camlSourceFile).mkString)
				}
				catch {
					case ex: FileNotFoundException =>
						error("Input file not found")
						None
				}
			} andThen { content =>
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
			} andThenChain { prog =>
				chain {
					try {
						Some(TypeInference.typeCheck(TypeInference.emptyEnv, prog.expr) _2)
					}
					catch {
						case TypeInference.UnificationError(msg, expr) =>
							error("Type checking failed")
							error(msg)
							prog.positions get expr foreach { p => error(p.toString) }
							None
						case TypeInference.TypeError(msg) =>
							error("Type checking failed")
							error(msg)
							None
					}
				} andThen { gamma =>
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
			} andFinally { case mama =>
				genByteCode(mama, jarFile)
			}
		}
	}
}
