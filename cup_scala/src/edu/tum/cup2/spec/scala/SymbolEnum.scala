package edu.tum.cup2.spec.scala {

import edu.tum.cup2.grammar.{NonTerminal, Terminal}

/**
 * Enum-Class for Terminal and NonTerminal Symbols. Provides methods
 * for the Constants within the enum so that they can be defined with
 * interface Terminal or NonTerminal. 
 * 
 * @author matthias
 *
 */
class SymbolEnum extends Enumeration {
	
  protected def TerminalEnum = new Val(newName()) with Terminal
  protected def NonTerminalEnum = new Val(newName()) with NonTerminal
  
  /**
   * Returns a new name, depending on the "enum"-Name
   */
  private def newName() = if(nextName.hasNext) nextName.next else null
}

}