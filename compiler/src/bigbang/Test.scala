package bigbang

import parser.Parser
import parser.ast.expressions.Expression
import parser.generator._
import typeinference.TypeInference
import codegen.mama.Translator
import codegen.mama.mamaInstructions.Instruction
import runtime.Assembly

import edu.tum.cup2.generator.LR1Generator
import edu.tum.cup2.parser.LRParser

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
			Parser.parse(input)._1
		
		def typeCheck(e:Expression):TypeInference.Env =
      TypeInference.typeCheck2(TypeInference.emptyEnv, e) _2
		
		def genMaMaCode(e:Expression):List[Instruction] =
			Translator.codeb(e,HashMap.empty,0)
		
		def genByteCode(l: List[Instruction], filename:String):Unit = {
			val jar = new Assembly(filename)
			jar.addManifest
			jar.copyClasses
			jar.injectCode(l)
			jar.close
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
			Translator.gamma = typeCheck(exp)
			val mamaCode = genMaMaCode(exp)
			output(mamaCode, x)
			genByteCode(mamaCode, "test/bigbang/" + x + ".jar")
		}
	}
}