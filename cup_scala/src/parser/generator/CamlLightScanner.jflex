package parser.generator;

%%

%class CamlLightScanner
%unicode
%cup2
%line
%column

%{
  private CamlLightSpec$Terminals$ terminals = CamlLightSpec.instance().terminals();

  private <T> ScannerToken<T> token(Object terminal, T value)
  {
    return new ScannerToken<T>((Terminal) terminal, value, yyline, yycolumn);
  }

  private ScannerToken<Object> token(Object terminal)
  {
    return new ScannerToken<Object>((Terminal) terminal, yyline, yycolumn);
  }
%}

LineTerminator = \r | \n | \r\n
InputCharacter = [^\r\n]
WhiteSpace     = {LineTerminator} | [ \t\f]

Comment = "(*" [^*] "*)"
//        | "(*" [^*] {Comment} [^*] "*)"

Identifier = [:jletter:] [:jletterdigit:]*

DecIntegerLiteral = "-"? [0-9]+

HexIntegerLiteral = "-"? 0 [xX] [0-9a-fA-F]+

OctIntegerLiteral = "-"? 0 [oO] [0-7]+

BinIntegerLiteral = "-"? 0 [bB] [0-1]+

%%

{Comment}	{ /* ignore */ }

{WhiteSpace}	{ /* ignore */ }

/* keywords */
//"and"		{ return token(); }
//"as"		{ return token(); }
//"begin"	{ return token(); }
//"do"		{ return token(); }
//"done"	{ return token(); }
//"downto"	{ return token(); }
"else"		{ return token(terminals.ELSE()); }
//"end"		{ return token(); }
//"exception"	{ return token(); }
//"for"		{ return token(); }
"fun"		{ return token(terminals.FUN()); }
"function"	{ return token(terminals.FUNCTION()); }
"if"		{ return token(terminals.IF()); }
"in"		{ return token(terminals.IN()); }
"let"		{ return token(terminals.LET()); }
"match"		{ return token(terminals.MATCH()); }
//"mutable"	{ return token(); }
"not"		{ return token(terminals.NOT()); }
"of"		{ return token(terminals.OF()); }
"or"		{ return token(terminals.OR()); }
//"prefix"	{ return token(); }
"rec"		{ return token(terminals.REC()); }
"then"		{ return token(terminals.THEN()); }
//"to"		{ return token(); }
//"try"		{ return token(); }
"type"		{ return token(terminals.TYPE()); }
//"value"	{ return token(); }
//"where"	{ return token(); }
//"while"	{ return token(); }
//"with"	{ return token(); }

//"#"		{ return token(); }
//"!"		{ return token(); }
//"!="		{ return token(); }
"&"		{ return token(terminals.AND()); }
"("		{ return token(terminals.LBRACKET()); }
")"		{ return token(terminals.RBRACKET()); }
//"*."		{ return token(); }
"*"		{ return token(terminals.MUL()); }
"+"		{ return token(terminals.PLUS()); }
//"+."		{ return token(); }
","		{ return token(terminals.COMMA()); }
"-"		{ return token(terminals.MINUS()); }
//"-."		{ return token(); }
//"->"		{ return token(); }
"."		{ return token(terminals.POINT()); }
//".("		{ return token(); }
"/"		{ return token(terminals.DIV()); }
//"/."		{ return token(); }
//":"		{ return token(); }
"::"		{ return token(terminals.CONS()); }
//":="		{ return token(); }
";"		{ return token(terminals.SEMI()); }
//";;"		{ return token(); }
"<"		{ return token(terminals.LESS()); }
//"<."		{ return token(); }
//"<-"		{ return token(); }
"<="		{ return token(terminals.LEQ()); }
//"<=."		{ return token(); }
"<>"		{ return token(terminals.NEQ()); }
//"<>."		{ return token(); }
"="		{ return token(terminals.BIND()); }
//"=."		{ return token(); }
"=="		{ return token(terminals.EQ()); }
">"		{ return token(terminals.GREATER()); }
//">."		{ return token(); }
">="		{ return token(terminals.GEQ()); }
//">=."		{ return token(); }
//"@"		{ return token(); }
"["		{ return token(terminals.LSQBRACKET()); }
//"[|"		{ return token(); }
"]"		{ return token(terminals.RSQBRACKET()); }
//"^"		{ return token(); }
//"_"		{ return token(); }
//"__"		{ return token(); }
"{"		{ return token(terminals.LBRACE()); }
"|"		{ return token(terminals.PIPE()); }
//"|]"		{ return token(); }
"}"		{ return token(terminals.RBRACE()); }
//"'"		{ return token(); }

{DecIntegerLiteral}	{ return token(terminals.INTCONST(), Integer.parseInt(yytext())); }

{Identifier}	{ return token(terminals.IDENTIFIER(), new String(yytext())); }

.		{ System.err.println("Error: Illegal character at line " + yyline + " and column " + yycolumn); }
