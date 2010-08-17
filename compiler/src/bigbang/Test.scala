package bigbang

import parser.ast.expressions.Expression
import parser.generator._
import typeinference.TypeInference
import codegen.mama.Translator
import codegen.mama.mamaInstructions.Instruction
import runtime.JarPacker._

import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.parser.LRParser

import java.io.StringReader
import java.io.FileWriter
import scala.io.Source
import scala.collection.immutable.HashMap

object Test {
def main(args:Array[String]):Unit = {
		val generator = new LR1Generator(CamlLightSpec)
		val table = generator.getParsingTable()
		val parser = new LRParser(table)
		
		val inputs = Source.fromFile("test/bigbang/test").getLines()
		
		def parseExp(input:String):Expression = 
			Normalizer.normalize(CamlLightSpec.parse(new StringReader(input))._1)
		
		def typeCheck(e:Expression):Unit = TypeInference.typeCheck(TypeInference.emptyEnv, e)
		
		def genMaMaCode(e:Expression):List[Instruction] =
			Translator.codeb(e,HashMap.empty,0)
		
		def genByteCode(l:List[Instruction],filename:String):Unit = {
			val jar = createJar(filename)
			addManifest(jar)
			copyClasses(jar)
			injectCode(jar)
			jar close
		}
		
		def output(l:List[Instruction],filename:String):Unit = {
			val out = new java.io.FileWriter("test/bigbang/" +	filename + ".mama")
			l map ((i:Instruction) => out write (i.toString))
			out close
		}
		
		inputs foreach { x => 
			val exp = parseExp(Source.fromFile("test/bigbang/" + x + ".cl").mkString(""));
			//Console println "***************************"
			//Console println exp
			typeCheck(exp)
			output(genMaMaCode(exp),x)
			//genByteCode(genMaMaCode(exp),"test/bigbang/" + x + ".jar")
		}
	}
}