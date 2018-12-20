package amyc
package analyzer

import utils._
import ast.{ Identifier, NominalTreeModule => N, SymbolicTreeModule => S }
/*import amyc.ast.TreeModule.AbstractClassDef
import amyc.ast.TreeModule.TypeTree
import amyc.ast.TreeModule.WildcardPattern*/

// Name analyzer for Amy
// Takes a nominal program (names are plain strings, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns the symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._

    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach {
      case (name, modules) =>
        if (modules.size > 1) {
          fatal(s"Two modules named $name in program", modules.head.position)
        }
    }

    modNames.keys.toList foreach table.addModule

    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given that we are within module 'inModule'.
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType     => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType  => S.StringType
        case N.UnitType    => S.UnitType
        case N.ClassTypeOrGeneric(qn @ N.QualifiedName(module, name), parametricTypes) =>
          table.getType(module getOrElse inModule, name) match {
            case Some((symbol, types)) =>
              if (parametricTypes.size != types.size) {
                error(s"Number of polymorphic types doesn't match; expected ${types.size}, actual ${parametricTypes.size}")
              }
              S.ClassType(symbol, parametricTypes.map(transformType(_, inModule)))
            case None =>
              if (!parametricTypes.isEmpty) {
                error(s"The class $name does not exist", tt)
              }
              S.GenericType(Identifier.fresh(name))
          }
      }
    }

    // Step 2: Check name uniqueness of definitions in each module
    p.modules.foreach {
      m =>
        m.defs.groupBy(_.name).foreach {
          case (name, defs) =>
            if (defs.size > 1) {
              fatal(s"Two defs named $name in program", defs.head.position)
            }
        }
    }

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach { m =>
      m.defs.foreach {
        _ match {
          case N.AbstractClassDef(name, polymorphicTypes) =>
            // TODO
            val t: List[S.GenericType] = polymorphicTypes.map(transformType(_, m.name) match {
              case s @ S.GenericType(_) => s
            })

            table.addType(m.name, name, t)

          /*  case N.CaseClassDef(name, fields, parent, polymorphicTypes) =>
          table.addType(m.name, name)*/
          case _ => //do nothing
        }

      }
    }

    // TODO faire un deuxième passage pour vérifier les types polymorphiques ne soient pas des types existants

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach { m =>
      m.defs.foreach { c =>
        c match {
          case N.CaseClassDef(name, fields, parent, polymorphicTypes, parentPolymorphicTypes) =>

            table.getType(m.name, parent) match {
              case Some(parentIdentifier) =>
                table.addConstructor(m.name, name, typeTreeToType(fields, m.name), parentIdentifier, polymorphicTypes)
              case None =>
                fatal(s"Could not find the parent class", c.position)
            }
          case _ => //do nothing
        }

      }
    }

    def typeTreeToType(l: List[N.TypeTree], inModule: String) = l.map(tt => transformType(tt, inModule))

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach { m =>
      m.defs.foreach { fun =>
        fun match {
          case N.FunDef(name, params, retType, body, polymorphicTypes) =>
            table.addFunction(m.name, name, typeTreeToType(params.map(_.tt), m.name), transformType(retType, m.name), polymorphicTypes)
          case _ => //do nothing
        }
      }

    }
    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions

    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // Keep in mind that we transform constructs of the NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = {
      df match {
        case N.AbstractClassDef(name) =>
          table.getType(module, name) match {
            case Some(t) => S.AbstractClassDef(t)
            case None    => fatal(s"Abstract type not defined", df)
          }
        case N.CaseClassDef(name, _, _) =>
          table.getConstructor(module, name) match {
            case Some((id, constrSig)) => S.CaseClassDef(id, constrSig.argTypes map S.TypeTree, constrSig.parent)
            case None                  => fatal(s"Case class type not defined", df)
          }
        case fd: N.FunDef =>
          transformFunDef(fd, module)
      }
    }.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach {
        case (name, ps) =>
          if (ps.size > 1) {
            fatal(s"Two parameters named $name in function ${fd.name}", fd)
          }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map {
        case (pd @ N.ParamDef(name, tt), tpe) =>
          val s = Identifier.fresh(name)
          S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)(implicit module: String, names: (Map[String, Identifier], Map[String, Identifier])): S.Expr = {
      val (params, locals) = names

      def getId(n: String): Option[Identifier] = names._2.get(n) match {
        case id @ Some(_) => id
        case None => names._1.get(n) match {
          case id @ Some(_) => id
          case None         => None
        }
      }

      def transformLiteral(lit: N.Literal[Any]) = lit match {
        case N.IntLiteral(value)     => S.IntLiteral(value)
        case N.BooleanLiteral(value) => S.BooleanLiteral(value)
        case N.StringLiteral(value)  => S.StringLiteral(value)
        case N.UnitLiteral()         => S.UnitLiteral()
        case _                       => fatal(s"This is not a literal", lit.position)
      }

      def transformParamDef(df: N.ParamDef) = {
        val id = Identifier.fresh(df.name)
        S.ParamDef(id, S.TypeTree(transformType(df.tt, module)))
      }

      val res = expr match {
        case N.Match(scrut, cases) =>
          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = pat match {
            case w @ N.WildcardPattern() => (S.WildcardPattern().setPos(w), Nil)
            case i @ N.IdPattern(name) =>

              if (locals.contains(name)) fatal(s"The name $name is already used by another variable.", pat)
              val id = Identifier.fresh(name)

              val constructorWithSameName = table.getConstructor(module, name) match {
                case Some((_, constrSig)) if (constrSig.argTypes.size == 0) => warning(s"You probably mean $name().", i)
                case _ =>
              }

              (S.IdPattern(id).setPos(i), List((name, id)))
            case l @ N.LiteralPattern(lit) => (S.LiteralPattern(transformLiteral(lit)).setPos(l), Nil)
            case caseClass @ N.CaseClassPattern(constr, args) =>
              val (id, constrSig) = table.getConstructor(constr.module.getOrElse(module),
                constr.name) match {
                  case Some(pair) => pair
                  case None       => fatal(s"Could not find the constructor", caseClass.position)
                }

              def combinePattern(list: List[(S.Pattern, List[(String, Identifier)])]): (List[S.Pattern], List[(String, Identifier)]) = {
                list match {
                  case (pattern, ids) :: rest =>
                    val (patterns, leftIds) = combinePattern(rest)
                    (pattern :: patterns, ids ::: leftIds)

                  case Nil => (Nil, Nil)
                }

              }

              val expectedSize = constrSig.argTypes.size
              val actualSize = args.size
              if (expectedSize != actualSize) fatal(s"Expected number of argument is $expectedSize but was $actualSize.", caseClass)

              val (patterns, mappings) = combinePattern(args.map(transformPattern))
              if (mappings.map(_._1).distinct.size != mappings.size)
                fatal(s"Two mappings are not distincts", pat.position)

              (S.CaseClassPattern(id, patterns).setPos(caseClass), mappings)
          }

          def transformCase(cse: N.MatchCase) = {
            val N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            val newLocals: Map[String, Identifier] = locals ++ moreLocals.toMap
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, newLocals))).setPos(cse)
          }

          S.Match(transformExpr(scrut), cases.map(transformCase)).setPos(expr)

        case t @ N.IntLiteral(value)     => S.IntLiteral(value).setPos(t)
        case t @ N.BooleanLiteral(value) => S.BooleanLiteral(value).setPos(t)
        case t @ N.StringLiteral(value)  => S.StringLiteral(value).setPos(t)
        case t @ N.UnitLiteral()         => S.UnitLiteral().setPos(t)
        case t @ N.Plus(lhs, rhs)        => S.Plus(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Minus(lhs, rhs)       => S.Minus(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Times(lhs, rhs)       => S.Times(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Div(lhs, rhs)         => S.Div(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Mod(lhs, rhs)         => S.Mod(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.LessThan(lhs, rhs)    => S.LessThan(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.LessEquals(lhs, rhs)  => S.LessEquals(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.And(lhs, rhs)         => S.And(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Or(lhs, rhs)          => S.Or(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Equals(lhs, rhs)      => S.Equals(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Concat(lhs, rhs)      => S.Concat(transformExpr(lhs), transformExpr(rhs)).setPos(t)
        case t @ N.Not(e)                => S.Not(transformExpr(e)).setPos(t)
        case t @ N.Neg(e)                => S.Neg(transformExpr(e)).setPos(t)

        case t @ N.Call(qname, args) =>
          val moduleName = qname.module.getOrElse(module)
          val name = qname.name
          val actualSize = args.size
          val newQname = table.getConstructor(moduleName, name) match {
            case Some((id, constrSig)) =>
              val expectedSize = constrSig.argTypes.size
              if (expectedSize != actualSize) fatal(s"Expected number of argument is $expectedSize but was $actualSize.", t)
              id
            case None => //maybe a function call
              table.getFunction(moduleName, name) match {
                case Some((id, funSig)) =>
                  val expectedSize = funSig.argTypes.size
                  if (expectedSize != actualSize) fatal(s"Expected number of argument is $expectedSize but was $actualSize.", t)
                  id
                case None => fatal(s"Could not find the identifier $name in module $moduleName.", t.position)
              }
          }
          S.Call(newQname, args.map(transformExpr)).setPos(t)
        case t @ N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2)).setPos(t)
        case t @ N.Let(df, value, body) =>
          val name = df.name
          if (locals.contains(name)) fatal(s"The name $name has already been defined in this scope", df.position)
          else if (params.contains(name)) warning(s"The variable $name is shadowing parameters.", df)
          val paramDef = transformParamDef(df)
          S.Let(paramDef, transformExpr(value), transformExpr(body)(module, (params, locals + (name -> paramDef.name))))
            .setPos(t)
        case t @ N.Ite(cond, thenn, elze) => S.Ite(transformExpr(cond), transformExpr(thenn), transformExpr(elze)).setPos(t)
        case t @ N.Error(msg)             => S.Error(transformExpr(msg)).setPos(t)
        case t @ N.Variable(name) =>
          val id = getId(name) match {
            case Some(i) => i
            case None    => fatal(s"The variable $name has not been declared before", t)
          }
          S.Variable(id).setPos(t)

        case _ => fatal(s"The expression is not a valid expression.", expr)
      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map {
        case mod @ N.ModuleDef(name, defs, optExpr) =>
          S.ModuleDef(
            table.getModule(name).get,
            defs map (transformDef(_, name)),
            optExpr map (transformExpr(_)(name, (Map(), Map())))).setPos(mod)
      }).setPos(p)

    (newProgram, table)

  }
}
