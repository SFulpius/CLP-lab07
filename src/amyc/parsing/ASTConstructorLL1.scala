package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  // TODO: Override methods from ASTConstructor as needed
  
  /* ... */
  override def constructType(pTree: NodeOrLeaf[Token]): TypeTree = {
    pTree match {
      case Node('Type ::= _, List(Leaf(tp))) =>
        TypeTree((tp: @unchecked) match {
          case INT() => IntType
          case STRING() => StringType
          case BOOLEAN() => BooleanType
          case UNIT() => UnitType
        }).setPos(tp)
      case Node('Type ::= _, List(id, idN, polymorphicTypesN)) =>
        val (qname, pos) = constructQname(id, idN)
        TypeTree(ClassTypeOrGeneric(qname ,
            constructParameterList(polymorphicTypesN))).setPos(pos)
    }
  }
  
  private def constructParameterList(pTree : NodeOrLeaf[Token]) : List[TypeTree] = pTree match {
    case Node(_ , List()) => Nil // no parametric types
    case Node('PolymorphicIdOrTypeListN ::= _, List(_, idOrType, list, _)) =>
      constructType(idOrType) :: constructList(list, constructType, true)
  }
  
  /* to allow polymorphic types */ // TODO
  override def constructDef0(pTree: NodeOrLeaf[Token]): ClassOrFunDef = {
  	pTree match {
      case Node('AbstractClassDef ::= _, List(Leaf(abs), _, name, polymorphicTypes)) =>
        AbstractClassDef(constructName(name)._1, constructGenericList(polymorphicTypes)).setPos(abs)
      case Node('CaseClassDef ::= _, List(Leaf(cse), _, name, polymorphicTypes , _, params, _, _, parent, parentPolymorphicTypes)) =>
        CaseClassDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true).map(_.tt),
          constructName(parent)._1,
          constructGenericList(polymorphicTypes),
          constructGenericList(parentPolymorphicTypes)
          ).setPos(cse)
      case Node('FunDef ::= _, List(Leaf(df), name,polymorphicTypes ,_, params, _, _, retType, _, _, body, _)) =>
        FunDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true),
          constructType(retType),
          constructExpr(body),
          constructGenericList(polymorphicTypes),
        ).setPos(df)
    }
  }
  
  private def constructGenericList(pTree : NodeOrLeaf[Token]) : List[TypeTree] = pTree match {
    case Node(_, List()) => Nil
    case Node('PolymorphicDefN ::= _, List(_, id, list, _)) => 
      def constructGeneric(id: NodeOrLeaf[Token]) =  {
        val (name, pos) = constructName(id)
        TypeTree(GenericType(name)).setPos(pos)
      }
      constructGeneric(id) :: constructList(list, constructGeneric, true)
  }
  
  
  
  def constructQname(id : NodeOrLeaf[Token], idN : NodeOrLeaf[Token]) : (QualifiedName, Positioned) = {
     idN match{
      case Node('IdN ::= _, List(_,optId)) =>
        val(module, pos) = constructName(id)
        val(name,_) = constructName(optId)
        (QualifiedName(Some(module),name),pos)
      case Node('IdN ::= _, List()) =>
        val (name, pos) = constructName(id)
        (QualifiedName(None, name),pos)
    }
  }
  
  def constructVal(ptree : NodeOrLeaf[Token]) : Expr = {
   ptree match{
     case Node('Val ::= _, List(_,param,_,p2Expr,_,expr)) =>
       val p = constructParam(param)
       val p2 = constructExprP2(p2Expr)
       val p1 = constructExpr(expr)
       Let(p, p2, p1).setPos(p)
   }
  }
  
  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match{
      case Node('Expr ::= List('P1Expr), List(p1ExprN)) =>
        constructExprP1(p1ExprN)
      case Node('Expr ::= List('Val), List(startVal)) =>
        constructVal(startVal)
    }
  }
  
  def constructCases(ptree : NodeOrLeaf[Token]) : List[MatchCase] = {
    ptree match{
      case Node('Cases ::= _ , List(c, Node(_, List()))) => //only one case
        constructCase(c) :: Nil
      case Node('Cases ::=_, List(c, cases)) => //more cases
        cases match {
          case Node('CaseN ::=_, List(cases)) =>
            constructCase(c) :: constructCases(cases)
        }
    }
  }
  
  def constructLiteralWithoutParen(pTree :NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('LitWithoutParen ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('LitWithoutParen ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('LitWithoutParen ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LitWithoutParen ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }
  
  override def constructLiteral(pTree : NodeOrLeaf[Token]) : Literal[_] = {
    pTree match{
      case Node('Literal ::= _, List(literal)) =>
        constructLiteralWithoutParen(literal)
      case Node('Literal ::= _, List(Leaf(lp@LPAREN()), Leaf(RPAREN()))) =>
        UnitLiteral().setPos(lp)
    }
  }
  
  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    (ptree : @unchecked) match {
      case Leaf(t) =>
        tokenToExpr(t)
    }
  }
  
  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::=_, List(id,Node(_,List()))) => //IdPattern
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
      case Node('Pattern ::=_, List(id,idPatternN)) =>
        idPatternN match {
          case Node('IdPatternN ::=_, List(idN,_,patterns,_)) => //case class
            val (name,pos) = constructQname(id, idN)
            val p = constructList(patterns, constructPattern, hasComma = true)
            CaseClassPattern(name,p).setPos(pos)
        }
    }
  }
  
  def constructExprP1(ptree : NodeOrLeaf[Token]): Expr = {
    ptree match{
      case Node('P1Expr ::= _, List(p2Expr,Node(_,List()))) => //No val or semicolon
        constructExprP2(p2Expr)
      case Node('P1Expr ::= _, List(p2Expr, p1ExprN)) => //val or semicolon
        p1ExprN match {
          case Node('P1ExprN ::= _, List(_, p1ExprNN)) =>
              val expr = constructExprP2(p2Expr)
              p1ExprNN match{
                case Node('P1ExprNN ::= List('Val), List(v)) => //local variable
                  Sequence(expr, constructVal(v)).setPos(expr)
                case Node('P1ExprNN ::= _, List(p1Expr)) => //sequence
                  Sequence(expr, constructExprP1(p1Expr)).setPos(expr)
              }
        }
    }
  }
  
  
  def constructExprP2(ptree : NodeOrLeaf[Token]): Expr = {
    ptree match{
      case Node('P2Expr ::= _, List(p3Expr, Node(_,List()))) => //no pattern matching
        constructExprP3(p3Expr)
      case Node('P2Expr ::= _, List(p3Expr, p2ExprN)) =>  
        p2ExprN match {
          case Node('P2ExprN ::= _ , List(_,_,cases,_)) =>
            val expr = constructExprP3(p3Expr)
            Match(expr, constructCases(cases)).setPos(expr)
        }
        
    }
  }
  
  def constructExprP3(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P3Expr ::= _, List(p4Expr, Node(_,List()))) =>
        constructExprP4(p4Expr)
      case Node('P3Expr ::= _, List(p4Expr, p3ExprN)) => 
        constructOpExpr(constructExprP4(p4Expr), p3ExprN)
    }
  }
  
  def constructExprP4(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P4Expr ::= _, List(p5Expr, Node(_,List()))) =>
        constructExprP5(p5Expr)
      case Node('P4Expr ::= _, List(p5Expr, p4ExprN)) =>  
        constructOpExpr(constructExprP5(p5Expr), p4ExprN)
    }
  }
  
  def constructExprP5(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P5Expr ::= _, List(p6Expr, Node(_,List()))) =>
        constructExprP6(p6Expr)
      case Node('P5Expr ::= _, List(p6Expr, p5ExprN)) =>
        constructOpExpr(constructExprP6(p6Expr), p5ExprN)
    }
  }
  
  def constructExprP6(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P6Expr ::= _, List(p7Expr, Node(_,List()))) =>
        constructExprP7(p7Expr)
      case Node('P6Expr ::= _, List(p7Expr, p6ExprN)) =>
        constructOpExpr(constructExprP7(p7Expr), p6ExprN)
    }
  }
  
  def constructExprP7(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P7Expr ::= _, List(p8Expr, Node(_,List()))) =>
        constructExprP8(p8Expr)
      case Node('P7Expr ::= _, List(p8Expr, p7ExprN)) =>
        constructOpExpr(constructExprP8(p8Expr), p7ExprN)
    }
  } 
  
  def constructExprP8(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P8Expr ::= _, List(p9Expr, Node(_,List()))) =>
        constructExprP9(p9Expr)
      case Node('P8Expr ::= _, List(p9Expr, p8ExprN)) =>  
        constructOpExpr(constructExprP9(p9Expr), p8ExprN) 
    }
  }
  
  def constructExprP9(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P9Expr ::= _, List(p10Expr)) =>
        constructExprP10(p10Expr)
      case Node('P9Expr ::= l, List(_, p10Expr)) =>
        val expr = constructExprP10(p10Expr)
        l match {
          case List(MINUS(), _) =>
            Neg(expr).setPos(expr)
          case List(BANG(), _) =>
            Not(expr).setPos(expr)
        }
    }
  }
  
  def constructExprP10(ptree : NodeOrLeaf[Token]) : Expr = {
    ptree match{
      case Node('P10Expr ::= _, List(_,_, expr1,_,_,expr2,_,_,_,expr3,_)) => //if-then-else
        val e1 = constructExpr(expr1)
        Ite(e1,constructExpr(expr2), constructExpr(expr3)).setPos(e1)
      case Node('P10Expr ::= _, List(_,_,expr,_)) =>  //error
        val e = constructExpr(expr)
        Error(e).setPos(e)
      case Node('P10Expr ::= _, List(Leaf(lp@LPAREN()),expr,_)) => //parentheses or unit
         expr match{
           case Node('OptExpr ::= _, List()) =>
             UnitLiteral().setPos(lp)
           case Node('OptExpr ::= _, List(expr)) =>
             constructExpr(expr)
         }
      case Node('P10Expr ::= _ , List(id, Node(_,List()))) => //id
        val (name, pos) = constructName(id)
        Variable(name).setPos(pos)
      case Node('P10Expr ::= _, List(id, idFunN)) => // call
        val (moduleOrId, pos) = constructName(id)
        idFunN match {
          case Node('IdCallN ::= _, List(types, _, args, _)) => // call without module name
            val qName = QualifiedName(None,moduleOrId)
            Call(qName, constructList(args, constructExpr, true, constructParameterList(types))).setPos(pos)
          case Node('IdCallN ::= _, List(_,id2, types, _,args,_)) => // call with module name
            val(name, pos2) = constructName(id2)
            val qName = QualifiedName(Some(moduleOrId), name)
            Call(qName, constructList(args, constructExpr, true, constructParameterList(types))).setPos(pos) 
        }
      case Node('P10Expr ::= _, List(litWithoutParen)) => //literal without unit
        constructLiteralWithoutParen(litWithoutParen)
        
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('P3ExprN, 'P4ExprN, 'P5ExprN, 'P6ExprN, 'P7ExprN, 'P8ExprN) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = nextOpd match {
              case Node('P3Expr ::= _,_) =>
                constructExprP3(nextOpd)
              case Node('P4Expr ::= _,_) =>
                constructExprP4(nextOpd)
              case Node('P5Expr ::= _,_) =>
                constructExprP5(nextOpd)
              case Node('P6Expr ::= _,_) =>
                constructExprP6(nextOpd)
              case Node('P7Expr ::= _,_) =>
                constructExprP7(nextOpd)
              case Node('P8Expr ::= _, _) =>
                constructExprP8(nextOpd)
              case Node('P9Expr ::= _,_) =>
                constructExprP9(nextOpd)
            }
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

}

