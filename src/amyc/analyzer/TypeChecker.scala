package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)
        
        case Variable(name) =>
          topLevelConstraint(env(name))
        
        case Equals(lhs, rhs) =>
          val typ = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ::: genConstraints(lhs,typ) ::: genConstraints(rhs,typ)
        case Plus(lhs,rhs) =>
          topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Minus(lhs,rhs) =>
          topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Times(lhs,rhs) =>
          topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Div(lhs,rhs) =>
          topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case Mod(lhs,rhs) =>
          topLevelConstraint(IntType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case LessThan(lhs,rhs) =>
          topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case LessEquals(lhs,rhs) =>
          topLevelConstraint(BooleanType) ::: genConstraints(lhs, IntType) ::: genConstraints(rhs, IntType)
        case And(lhs,rhs) =>
          topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
        case Or(lhs,rhs) =>
          topLevelConstraint(BooleanType) ::: genConstraints(lhs, BooleanType) ::: genConstraints(rhs, BooleanType)
        case Concat(lhs,rhs) =>
          topLevelConstraint(StringType) ::: genConstraints(lhs, StringType) ::: genConstraints(rhs, StringType)
        case Not(e) =>
          topLevelConstraint(BooleanType) ::: genConstraints(e, BooleanType)
        case Neg(e) =>
          topLevelConstraint(IntType) ::: genConstraints(e, IntType)
        
        case Call(id, args) =>
          val (argTypes, returnType) = table.getConstructor(id) match {
            case Some(constr) => (constr.argTypes, constr.retType)
            case None => 
              val funDef = table.getFunction(id).get
              (funDef.argTypes, funDef.retType)
          }
          topLevelConstraint(returnType) ::: args.zip(argTypes).flatMap(pair => genConstraints(pair._1, pair._2))
        case Sequence(e1, e2) =>
          val typeE2 = TypeVariable.fresh()
          topLevelConstraint(typeE2) ::: genConstraints(e2, typeE2) ::: genConstraints(e1, TypeVariable.fresh)
        case Let(df, value, body) =>
          val typeBody = TypeVariable.fresh()
          topLevelConstraint(typeBody) ::: genConstraints(value, df.tt.tpe)  ::: genConstraints(body, typeBody)(env + (df.name -> df.tt.tpe))
        case Ite(cond, thenn, elze) =>
          val returnType = TypeVariable.fresh()
          topLevelConstraint(returnType) ::: genConstraints(cond, BooleanType) ::: genConstraints(thenn, returnType) ::: genConstraints(elze, returnType)
        case Error(msg) =>
          topLevelConstraint(TypeVariable.fresh()) ::: genConstraints(msg, StringType)
          
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              case WildcardPattern() => 
                (List(), Map.empty)
              case IdPattern(name) => 
                val typ = TypeVariable.fresh()
                (List(Constraint(typ, scrutExpected, pat.position)), Map(name -> typ))
              case LiteralPattern(literal) =>
                (genConstraints(literal, scrutExpected), Map.empty)
              case CaseClassPattern(constr, args) =>
                val constrSignature = table.getConstructor(constr).get
                val typeConstraint = Constraint(constrSignature.retType, scrutExpected, pat.position)
                val t = args.zip(constrSignature.argTypes)
                .map(p => handlePattern(p._1, p._2))
                .unzip
                 (typeConstraint :: t._1.flatten , t._2.foldLeft(Map.empty: Map[Identifier, Type])((acc,el) => acc ++ el))
             }
          }

          def handleCase(cse: MatchCase, scrutExpected: Type, returnExpected : Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            genConstraints(cse.expr, returnExpected)(env ++ moreEnv) ::: patConstraints
          }

          val st = TypeVariable.fresh()
          val returnExpected = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st, returnExpected)) ++ topLevelConstraint(returnExpected)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          found match {
            case TypeVariable(id) =>
              solveConstraints(subst_*(more, id, expected))
            case _ => expected match {
              case TypeVariable(id) => 
                solveConstraints(subst_*(more, id, found))
              case _ =>
                if(found != expected) 
                  error("The types doesn't match. Expected " + expected.toString() + " but was " + found.toString() + ".", pos)
                solveConstraints(more)
            }
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
