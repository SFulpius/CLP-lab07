package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // TODO: Write a grammar that implements the correct syntax of Amy and is LL1.
  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Expr ::= 'P1Expr | 'Val,
    'P1Expr ::= 'P2Expr ~ 'P1ExprN,
    'P1ExprN ::= SEMICOLON() ~ 'P1ExprNN | epsilon(),
    'P1ExprNN ::= 'Val | 'P1Expr,
    'P2Expr ::= 'P3Expr ~ 'P2ExprN,
    'P2ExprN ::=  MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | epsilon(),
    'P3Expr ::= 'P4Expr ~ 'P3ExprN,
    'P3ExprN ::= OR() ~ 'P3Expr | epsilon(),
    'P4Expr ::= 'P5Expr ~ 'P4ExprN,
    'P4ExprN ::= AND() ~ 'P4Expr | epsilon(),
    'P5Expr ::= 'P6Expr ~ 'P5ExprN,
    'P5ExprN ::= EQUALS() ~ 'P5Expr | epsilon(),
    'P6Expr ::= 'P7Expr ~ 'P6ExprN,
    'P6ExprN ::=  LESSTHAN() ~ 'P6Expr| LESSEQUALS() ~ 'P6Expr | epsilon(),
    'P7Expr ::= 'P8Expr ~ 'P7ExprN,
    'P7ExprN ::= PLUS() ~ 'P7Expr | MINUS() ~ 'P7Expr | CONCAT() ~ 'P7Expr | epsilon(),
    'P8Expr ::= 'P9Expr ~ 'P8ExprN,
    'P8ExprN ::= TIMES() ~ 'P8Expr | DIV() ~ 'P8Expr | MOD() ~ 'P8Expr | epsilon(),
    'P9Expr ::= 'P10Expr | MINUS() ~'P10Expr | BANG() ~ 'P10Expr,
    'P10Expr ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'OptExpr ~ RPAREN() |
              'Id  ~ 'IdFunN | 'LitWithoutParen,
    'IdN ::= DOT() ~ 'Id | epsilon(),
    'IdFunN ::= DOT() ~ 'Id ~ 'PolymorphicTypeN ~ LPAREN() ~ 'Args ~ RPAREN() | 'PolymorphicTypeN ~ LPAREN() ~ 'Args ~ RPAREN() | epsilon(),
    'PolymorphicTypeN ::= epsilon() | LBRACKET() ~ 'Type ~ 'TypeN ~ RBRACKET(),
    'TypeN ::= COMMA() ~ 'Type ~ 'TypeN | epsilon(), 
    'Val ::= VAL() ~ 'Param ~ EQSIGN() ~ 'P2Expr ~ SEMICOLON() ~ 'Expr,
    
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT,
    'LitWithoutParen ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'Literal ::= 'LitWithoutParen | LPAREN() ~ RPAREN(),
    'Cases ::= 'Case ~ 'CaseN,
    'CaseN ::= 'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal |  'Id ~ 'IdPatternN,
    'IdPatternN ::=  'IdN ~LPAREN() ~ 'Patterns ~ RPAREN() | epsilon(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    
     'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id ~ 'PolymorphicDefN,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ 'PolymorphicDefN ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id ~ 'PolymorphicDefN,
    'FunDef ::= DEF() ~ 'Id ~ 'PolymorphicDefN ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'PolymorphicDefN ::= epsilon() | LBRACKET() ~ 'Id ~ 'PolymorphicIdN ~ RBRACKET(),
    'PolymorphicIdN ::= epsilon() | COMMA() ~ 'Id ~ 'PolymorphicIdN,
    'PolymorphicIdOrTypeN ::= epsilon() | LBRACKET() ~ 'IdOrType ~ 'PolymorphicIdOrTypeListN ~ RBRACKET(),
    'PolymorphicIdOrTypeListN ::= epsilon() | COMMA() ~ 'IdOrType ~ 'PolymorphicIdOrTypeListN,
    'IdOrType ::= 'Id | 'Type,
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'Id ~ 'IdN ~ 'PolymorphicIdOrTypeN
  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    // TODO: Switch to LL1 when you are ready
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
         //info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
