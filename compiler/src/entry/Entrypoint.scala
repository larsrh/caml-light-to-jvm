package entry

import scala.io.Source
import scala.collection.immutable.HashMap

import java.io.{StringReader,FileNotFoundException}

import org.github.scopt._

import parser.generator.Normalizer
import parser.generator.CamlLightSpec
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

	def main(args: Array[String]) {
		val config = new TypedMap[Key]
		val outputFile = Key[String]
		val inputFile = Key[String]
		val unChecked = Key[Boolean]

		val parser = new OptionParser("compiler") {
			opt("o", "output", "the output file, default to a.jar",
				{v: String => config.update(outputFile, v)})
			booleanOpt("t", "typecheck", "whether to activate type checking (default true)",
				{v: Boolean => config.update(unChecked, v)})
			arg("file", "CamlLight source code",
				{v: String => config.update(inputFile, v)})
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

			val typeCheck = config.get(unChecked) match {
				case Some(value) => value
				case None => false
			}

			try {
				val content = Source.fromFile(camlSourceFile).mkString("")
				val exp = Normalizer.normalize(CamlLightSpec.parse(new StringReader(content))._1)
				if (typeCheck)
					TypeInference.typeCheck(TypeInference.emptyEnv, exp)
				val mama = Translator.codeb(exp, HashMap.empty, 0)
				genByteCode(mama, jarFile)
			} catch {
				case ex: FileNotFoundException => {
					error("Input file not found")
					return
				}
			}
		}
	}
}
