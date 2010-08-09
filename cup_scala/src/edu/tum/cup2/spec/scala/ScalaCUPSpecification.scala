package edu.tum.cup2.spec.scala {

import edu.tum.cup2.grammar.{NonTerminal, Terminal, Symbol, Production}
import edu.tum.cup2.spec.util.{RHSSymbols, RHSItem}
import edu.tum.cup2.util.{Reflection}
import edu.tum.cup2.semantics.{Action, SymbolValueClasses, SymbolValue}
import edu.tum.cup2.spec.exceptions.{IllegalSpecException}
import edu.tum.cup2.spec.{CUP2Specification}

import collection.JavaConversions._


/**
 * Scala extension for CUPSpecification to allow grammar definitions in scala.
 * 
 * The specification will be in a the way of the ll parser combinators which are
 * included in the standard scala library. But in combination with the cup2
 * framework it will be possible to create a grammar for lr parsing, which allows
 * more language grammars to be parsed and therefore is mightier.
 * 
 * To use it, just mix this trait in your spec-class which already inherits from
 * CUPSpecification. 
 * 
 * @author matthias
 */
trait ScalaCUPSpecification { self : CUP2Specification =>

  /** Define the enum with Terminals-Symbols */
  val terminals : SymbolEnum
  /** Define the enum with NonTerminals-Symbols */
  val nonTerminals : SymbolEnum
  
  /** Make it easy to access terminals as Array */
  def terminalsArr : Array[Terminal] = Array[Terminal]() ++ (terminals.values map ( _.asInstanceOf[Terminal] ))
  /** Make it easy to access nonterminals as Array */
  def nonTerminalsArr = Array[NonTerminal]() ++ (nonTerminals.values map ( _.asInstanceOf[NonTerminal] ))
      
  
  /**
   * Override this method of super-class to get rid of the in scala non-available
   * enums as well as the somewhat nasty reflection usage.
   */
  override def init() = {
    if (! isInit) {
      symbolValueClasses = Reflection.getSymbolValueClasses(terminalsArr, nonTerminalsArr)
      //inited
      isInit = true
    }
  }
  
  /**
   * Just a redirect so that you can use scala collection type Seq for definition.
   */
  def production(nt : NonTerminal, items : Seq[RHSItem]) = self.prod(nt, items : _*)
  
  
  /**
   * Override Method getTerminals() from CUPSpecification to avoid terminal-determination
   * over reflection.
   */
  override def getTerminals() : Array[Terminal] = terminalsArr
  /**
   * Override Method getNonTerminals() from CUPSpecification to avoid nonterminal-determination
   * over reflection.
   */
  override def getNonTerminals() : Array[NonTerminal] = nonTerminalsArr
  

  
  implicit def item2items(s : Seq[RHSItem]) = new ParserCombinator(s)
  implicit def sym2syms(s : RHSSymbols) = new RHSSymbolsExt(s)
  implicit def sym2syms(s : Symbol) = new RHSSymbolsExt(new RHSSymbols(s))
  
  /**
   * Allows to construct a production by writing "nonterminal -> ..." where ... stands
   * for a Seq of RHSItems.
   */
  implicit def nonterm2prod(nt : NonTerminal) = new {
	  def ->(items : Seq[RHSItem]) = self.prod(nt, items : _*)
  }
  
  
  
  
  /**
   * Make it possible that every Symbol can be part of a parser combinator. That
   * means that it can be used to construct a production for a grammar.
   */
  class ParserCombinator(i : Seq[RHSItem]) {
    def | (o : Seq[RHSItem]) : Seq[RHSItem]       = i ++ o
    def | (f : AnyRef) : Seq[RHSItem]      = i ++ Seq(new RHSSymbols(), new ScalaAction(f))
  }

  /**
   * Extends RHSSymbols class to be combinable.
   */
  class RHSSymbolsExt(i : RHSSymbols) {
    def ~ (s : Symbol) : RHSSymbols        = new RHSSymbols( (i.getSymbols() :+ s) : _* )
  
    /**
     * Old java-version of an action-definition in CUP2.
     * @param a
     * @return
     */
    @deprecated("use ^^(f : AnyRef) instead!")
    def ^^(a : Action) : Seq[RHSItem]     = Seq(i, a)
    /**
     * New scala and functional version of action-definition. AnyRef has to be
     * a object with an appropriate apply()-method. So anonymous defined functions
     * like <code>(x : Int) => x</code> will fit!
     * @param f
     * @return
     */
    def ^^(f : AnyRef) : Seq[RHSItem]      = ^^(new ScalaAction(f))
  }
  
  
  /**
   * Override Method to check action not over the default Reflection-Method.
   * Try to imitate the behaviour of the default method but with respect of
   * the scala function object.
   */
  override def checkAction(action : Action, position : Int,
	  rhsSymbols : java.util.List[Symbol], symbolValueClasses : SymbolValueClasses) = {
	
	// surely possible cast
	var a = action.asInstanceOf[ScalaAction]
	
	// get real method to work with
	var method = a.searchMethod()
	
	try {
		edu.tum.cup2.util.Reflection.checkParamsOfAction(method, position, rhsSymbols, symbolValueClasses)
	} catch {
		case ex => ex.printStackTrace();
			throw new IllegalSpecException(ex.getMessage());
	}
  }
}

}