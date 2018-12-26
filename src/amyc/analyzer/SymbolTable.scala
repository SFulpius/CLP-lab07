package amyc.analyzer

import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule._
import amyc.utils.UniqueCounter

import scala.collection.mutable.HashMap

trait Signature[RT <: Type]{
  val argTypes: List[Type]
  val retType: RT
}
// The signature of a function in the symbol table
case class FunSig(argTypes: List[Type], retType: Type, owner: Identifier, polymorphicTypes : List[GenericType]) extends Signature[Type]
// The signature of a constructor in the symbol table
case class ConstrSig(argTypes: List[Type], parent: Identifier, index: Int, polymorphicTypes : List[GenericType]) extends Signature[ClassType] {
  val retType = ClassType(parent, polymorphicTypes.map(TypeTree))
}

// A class that represents a dictionary of symbols for an Amy program
class SymbolTable {
  private val defsByName = HashMap[(String, String), Identifier]()
  private val modules = HashMap[String, Identifier]()

  /**
   * Map from type identifier to their modules.
   */
  private val types = HashMap[Identifier, Identifier]()
  private val defsToPType = HashMap[Identifier, List[GenericType]]()
  private val functions = HashMap[Identifier, FunSig]()
  private val constructors = HashMap[Identifier, ConstrSig]()

  private val typesToConstructors = HashMap[Identifier, List[Identifier]]()

  private val constrIndexes = new UniqueCounter[Identifier]

  def addModule(name: String) = {
    val s = Identifier.fresh(name)
    modules += name -> s
    s
  }
  def getModule(name: String) = modules.get(name)

  def addType(owner: String, name: String, pTypes: List[GenericType]) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    types += (s -> modules.getOrElse(owner, sys.error(s"Module $name not found!")))
    defsToPType += (s -> pTypes)
    s
  }
  def getType(owner: String, name: String) =
    defsByName.get(owner,name) filter types.contains
  def getType(symbol: Identifier) = types.get(symbol)

  def addConstructor(owner: String, name: String, argTypes: List[Type], parent: Identifier, polymorphicTypes : List[GenericType]) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    if (defsToPType(parent).size != polymorphicTypes.size) {
      sys.error("the number of polymorphic types don't match")
    }
    constructors += s -> ConstrSig(
      argTypes,
      parent,
      constrIndexes.next(parent),
      polymorphicTypes)
    typesToConstructors += parent -> (typesToConstructors.getOrElse(parent, Nil) :+ s)
    defsToPType += (s -> polymorphicTypes)
    s
  }
  def getConstructor(owner: String, name: String): Option[(Identifier, ConstrSig)] = {
    for {
      sym <- defsByName.get(owner, name)
      sig <- constructors.get(sym)
    } yield (sym, sig)
  }
  def getConstructor(symbol: Identifier) = constructors.get(symbol)

  def getConstructorsForType(t: Identifier) = typesToConstructors.get(t)

  def addFunction(owner: String, name: String, argTypes: List[Type], retType: Type, polymorphicTypes : List[GenericType]) = {
    val s = Identifier.fresh(name)
    defsByName += (owner, name) -> s
    functions += s -> FunSig(argTypes, retType, getModule(owner).getOrElse(sys.error(s"Module $owner not found!")),
        polymorphicTypes)
    defsToPType += (s -> polymorphicTypes)
    s
  }
  def getFunction(owner: String, name: String): Option[(Identifier, FunSig)] = {
    for {
      sym <- defsByName.get(owner, name)
      sig <- functions.get(sym)
    } yield (sym, sig)
  }
  def getFunction(symbol: Identifier) = functions.get(symbol)
  
  def checkPType(module : String, pType : String) = defsByName.contains((module, pType))

}
