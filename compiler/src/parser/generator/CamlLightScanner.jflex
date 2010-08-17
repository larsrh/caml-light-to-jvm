package parser.generator;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static parser.generator.CamlLightTerminals.*;

%%

%class CamlLightScanner
%unicode
%cup2
%public
%line
%column
%state CHARACTER COMMENT STRING

%{
  private StringBuffer string = new StringBuffer();

  private int nested_comment_counter = 0;

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

  private char charFrom3Ints(String str)
  {
    Pattern p = Pattern.compile("\\\\([0-9])([0-9])([0-9])");
    Matcher m = p.matcher(str);
    m.matches();

    int a = Integer.parseInt(m.group(1));
    int b = Integer.parseInt(m.group(2));
    int c = Integer.parseInt(m.group(3));

    return (char)(a * 100 + b * 10 + c);
  }

  private char lexCharacter1(String str)
  {
    Pattern p = Pattern.compile("'([a-zA-Z0-9])'");
    Matcher m = p.matcher(str);
    m.matches();

    return m.group(1).charAt(0);
  }

  private char lexCharacter2(String str)
  {
    Pattern p = Pattern.compile("'\\\\([0-9])([0-9])([0-9])'");
    Matcher m = p.matcher(str);
    m.matches();

    int a = Integer.parseInt(m.group(1));
    int b = Integer.parseInt(m.group(2));
    int c = Integer.parseInt(m.group(3));

    return (char)(a * 100 + b * 10 + c);
  }

  private char lexCharacter3(String str)
  {
    Pattern p = Pattern.compile("'\\([tnrb\\])'");
    Matcher m = p.matcher(str);
    m.matches();

    switch (m.group(1).charAt(0)) {
      case 't': return '\t';
      case 'n': return '\n';
      case 'r': return '\r';
      case 'b': return '\b';
      case '\\': return '\\';
      default: throw new IllegalArgumentException("Error: Should not happen, but keeps Java quiet.");
    }
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
  "and"		{ return token(LETAND()); }
  "else"	{ return token(ELSE()); }
  "false"	{ return token(BOOLCONST(), false); }
  "fun"		{ return token(FUN()); }
  "function"	{ return token(FUNCTION()); }
  "if"		{ return token(IF()); }
  "in"		{ return token(IN()); }
  "let"		{ return token(LET()); }
  "match"	{ return token(MATCH()); }
  "not"		{ return token(NOT()); }
  "of"		{ return token(OF()); }
  "or"		{ return token(OR()); }
  "rec"		{ return token(REC()); }
  "with"	{ return token(WITH()); }
  "then"	{ return token(THEN()); }
  "true"	{ return token(BOOLCONST(), true); }
  "type"	{ return token(TYPE()); }

  ";;"		{ return token(SEMISEMI()); }
  "&"		{ return token(AND()); }
  "("		{ return token(LBRACKET()); }
  ")"		{ return token(RBRACKET()); }
  "*"		{ return token(STAR()); }
  "+"		{ return token(PLUS()); }
  ","		{ return token(COMMA()); }
  "-"		{ return token(MINUS()); }
  "."		{ return token(POINT()); }
  "/"		{ return token(SLASH()); }
  "::"		{ return token(CONS()); }
  ";"		{ return token(SEMI()); }
  "<"		{ return token(LESS()); }
  "<="		{ return token(LEQ()); }
  "<>"		{ return token(NEQ()); }
  "="		{ return token(BIND()); }
  "=="		{ return token(EQ()); }
  ">"		{ return token(GREATER()); }
  ">="		{ return token(GEQ()); }
  "["		{ return token(LSQBRACKET()); }
  "]"		{ return token(RSQBRACKET()); }
  "{"		{ return token(LBRACE()); }
  "|"		{ return token(PIPE()); }
  "}"		{ return token(RBRACE()); }
  "->"		{ return token(ARROW()); }
  "_"		{ return token(UNDERSCORE()); }
  "@"		{ return token(TUPLEACC()); }

  {DecIntegerLiteral}	{ return token(INTCONST(), Integer.parseInt(yytext())); }

  {HexIntegerLiteral}	{ return token(INTCONST(), parseHexInt(yytext())); }

  {OctIntegerLiteral}	{ return token(INTCONST(), parseOctInt(yytext())); }

  {BinIntegerLiteral}	{ return token(INTCONST(), parseBinInt(yytext())); }

  {Identifier}	{ return token(IDENTIFIER(), yytext()); }

  \'{Identifier}	{ return token(SQIDENTIFIER(), yytext()); }

  \'[a-zA-Z0-9]\'	{ return token(CHARCONST(), lexCharacter1(yytext())); }

  \'\\[0-9]{3}\'	{ return token(CHARCONST(), lexCharacter2(yytext())); }

  \'\\t\'		{ return token(CHARCONST(), lexCharacter3(yytext())); }

  \'\\n\'		{ return token(CHARCONST(), lexCharacter3(yytext())); }

  \'\\r\'		{ return token(CHARCONST(), lexCharacter3(yytext())); }

  \'\\b\'		{ return token(CHARCONST(), lexCharacter3(yytext())); }

  \'\\\'		{ return token(CHARCONST(), lexCharacter3(yytext())); }

  \"		{ string.setLength(0); yybegin(STRING); }

  .		{ throw new IllegalArgumentException("Error: Illegal character at line " + (yyline+1) + " and column " + yycolumn); }
}

<STRING> {
  \"		{ yybegin(YYINITIAL); return token(STRINGCONST(), string.toString()); }
  \\[0-9]{3}	{ string.append(charFrom3Ints(yytext())); }
  [^\n\r\"\\]+	{ string.append( yytext() ); }
  \\t		{ string.append('\t'); }
  \\n		{ string.append('\n'); }
  \\r		{ string.append('\r'); }
  \\b		{ string.append('\b'); }
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
  {WhiteSpace}	{ /* ignore */ }
  .		{ /* ignore */ }
}
