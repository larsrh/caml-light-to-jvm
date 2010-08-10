package parser.generator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

%%

%class CamlLightScanner
%unicode
%cup2
%line
%column
%state STRING COMMENT

%{
  private StringBuffer string = new StringBuffer();

  private int nested_comment_counter = 0;

  private CamlLightSpec$Terminals$ terminals = CamlLightSpec.terminals();

  private <T> ScannerToken<T> token(Object terminal, T value)
  {
    return new ScannerToken<T>((Terminal) terminal, value, yyline+1, yycolumn);
  }

  private ScannerToken<Object> token(Object terminal)
  {
    return new ScannerToken<Object>((Terminal) terminal, yyline+1, yycolumn);
  }

  private int parseHexInt(String str)
  {
    Pattern p = Pattern.compile("0[xX]([0-9a-fA-F]+)");
    Matcher m = p.matcher(str);
    m.matches();
    return Integer.parseInt(m.group(1), 16);
  }

  private int parseOctInt(String str)
  {
    Pattern p = Pattern.compile("0[oO]([0-7]+)");
    Matcher m = p.matcher(str);
    m.matches();
    return Integer.parseInt(m.group(1), 8);
  }

  private int parseBinInt(String str)
  {
    Pattern p = Pattern.compile("0[bB]([0-1]+)");
    Matcher m = p.matcher(str);
    m.matches();
    return Integer.parseInt(m.group(1), 2);
  }
%}

LineTerminator = \r | \n | \r\n

WhiteSpace = {LineTerminator} | [ \t\f]

Identifier = [:jletter:] [:jletterdigit:]*

DecIntegerLiteral = [0-9]+

HexIntegerLiteral = 0 [xX] [0-9a-fA-F]+

OctIntegerLiteral = 0 [oO] [0-7]+

BinIntegerLiteral = 0 [bB] [0-1]+

%%

<YYINITIAL> {
  "(*"		{ ++nested_comment_counter; yybegin(COMMENT); }
  "*)"		{ throw new IllegalArgumentException("Error: Wrong count of closing comments."); }

  {WhiteSpace}	{ /* ignore */ }

  /* keywords */
  "and"		{ return token(terminals.LETAND()); }
  "else"	{ return token(terminals.ELSE()); }
  "false"	{ return token(terminals.FALSE()); }
  "fun"		{ return token(terminals.FUN()); }
  "function"	{ return token(terminals.FUNCTION()); }
  "if"		{ return token(terminals.IF()); }
  "in"		{ return token(terminals.IN()); }
  "let"		{ return token(terminals.LET()); }
  "match"	{ return token(terminals.MATCH()); }
  "not"		{ return token(terminals.NOT()); }
  "of"		{ return token(terminals.OF()); }
  "or"		{ return token(terminals.OR()); }
  "rec"		{ return token(terminals.REC()); }
  "then"	{ return token(terminals.THEN()); }
  "true"	{ return token(terminals.TRUE()); }
  "type"	{ return token(terminals.TYPE()); }

  "&"		{ return token(terminals.AND()); }
  "("		{ return token(terminals.LBRACKET()); }
  ")"		{ return token(terminals.RBRACKET()); }
  "*"		{ return token(terminals.MUL()); }
  "+"		{ return token(terminals.PLUS()); }
  ","		{ return token(terminals.COMMA()); }
  "-"		{ return token(terminals.MINUS()); }
  "."		{ return token(terminals.POINT()); }
  "/"		{ return token(terminals.DIV()); }
  "::"		{ return token(terminals.CONS()); }
  ";"		{ return token(terminals.SEMI()); }
  "<"		{ return token(terminals.LESS()); }
  "<="		{ return token(terminals.LEQ()); }
  "<>"		{ return token(terminals.NEQ()); }
  "="		{ return token(terminals.BIND()); }
  "=="		{ return token(terminals.EQ()); }
  ">"		{ return token(terminals.GREATER()); }
  ">="		{ return token(terminals.GEQ()); }
  "["		{ return token(terminals.LSQBRACKET()); }
  "]"		{ return token(terminals.RSQBRACKET()); }
  "{"		{ return token(terminals.LBRACE()); }
  "|"		{ return token(terminals.PIPE()); }
  "}"		{ return token(terminals.RBRACE()); }

  {DecIntegerLiteral}	{ return token(terminals.INTCONST(), Integer.parseInt(yytext())); }

  {HexIntegerLiteral}	{ return token(terminals.INTCONST(), parseHexInt(yytext())); }

  {OctIntegerLiteral}	{ return token(terminals.INTCONST(), parseOctInt(yytext())); }

  {BinIntegerLiteral}	{ return token(terminals.INTCONST(), parseBinInt(yytext())); }

  {Identifier}	{ return token(terminals.IDENTIFIER(), new String(yytext())); }

  \"		{ string.setLength(0); yybegin(STRING); }

  .		{ throw new IllegalArgumentException("Error: Illegal character at line " + (yyline+1) + " and column " + yycolumn); }
}

<STRING> {
  \"		{ yybegin(YYINITIAL); return token(terminals.STRING(), string.toString()); }
  [^\n\r\"\\]+	{ string.append( yytext() ); }
  \\t		{ string.append('\t'); }
  \\n		{ string.append('\n'); }
  \\r		{ string.append('\r'); }
  \\\"		{ string.append('\"'); }
  \\		{ string.append('\\'); }
}

<COMMENT> {
  "(*"		{ ++nested_comment_counter; }
  "*)"		{ if (nested_comment_counter == 1)
			yybegin(YYINITIAL);
		  --nested_comment_counter;
		}
  <<EOF>>	{ throw new IllegalArgumentException("Error: Wrong count of opening comments."); }
  .		{ /* ignore */ }
}
