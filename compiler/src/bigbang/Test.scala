package bigbang

import parser.ast.expressions.Expression
import parser.generator._
import codegen.mama._
import codegen.mama.mamaInstructions.Instruction

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
			Normalizer.normalize(parser.parse(new CamlLightScanner(new StringReader(input))).asInstanceOf[Expression])
		
		//TODO def typeCheck(e:Expression):?
		
		def genMaMaCode(e:Expression):List[Instruction] =
			Translator.codeb(e,HashMap.empty,0)
		
		//TODO def genByteCode(l:List[Instruction]):Unit
		
		def output(l:List[Instruction],filename:String):Unit = {
			val out = new java.io.FileWriter("test/bigbang/" +	filename + ".mama")
			l map ((i:Instruction) => out write (i.toString))
			out close
		}
		
		inputs foreach { x => 
			val exp = parseExp(Source.fromFile("test/bigbang/" + x + ".cl").mkString(""));
			//Console println exp
			output(genMaMaCode(exp),x)
		}
	}
}