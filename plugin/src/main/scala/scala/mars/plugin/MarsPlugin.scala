package scala.mars.plugin

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

object MarsPlugin {
  val expandNameOpt = "expandName"
}

class MarsPlugin(val global: Global) extends Plugin {
  import MarsPlugin._
  import global._
  
  val name = "marsplugin"
  val description = "runtime macro expansion plugin"
  var expandName = "test"
  val components = List[PluginComponent](MarsComponent)

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.startsWith(expandNameOpt)) {
        expandName = option.substring(expandNameOpt.length)
      } else{
          error("Option not understood: "+option)
      }
    }
  }

  private object MarsComponent extends PluginComponent {
    val global = MarsPlugin.this.global
    import global._
    import global.analyzer.Context

    override val runsAfter = List("typer")

    val phaseName = "marsPhase"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(compUnit: CompilationUnit) {
        try {
          //process only .scala files
          val fileName = compUnit.source.file.name
          if (fileName.endsWith(".scala")) {
            val tree = compUnit.body
            println("unit: " + compUnit)
            val initialContext = analyzer.rootContext(compUnit, EmptyTree, false)
            val contextCreator = new ContextCreator(initialContext)
            println(showCode(tree))
          } else
            println("File " + fileName + " is not processed")
          } catch {
            case e: Exception =>
              e.printStackTrace()
              throw e
          }
      }
    }
    
    object ContextCreator {
      def apply(context: Context) = 
        new ContextCreator(context)
    }
    
    class ContextCreator(var context: Context) {
      
      def printScopeInfo = {
        println(s"context: $context")
        println(s"scope: ${context.scope}")
      }
      
      import scala.collection.mutable.ListBuffer
      var treeContexts: ListBuffer[(Tree, Context)] = ListBuffer.empty
      def addToContexts(tree: Tree, context: Context) = { treeContexts += ((tree, context)) }
      
      def contextedStats(stats: List[Tree], exprOwner: Symbol = NoSymbol): Unit = {
        val inBlock = exprOwner == context.owner
        def includesTargetPos(tree: Tree) =
          tree.pos.isRange && context.unit.exists && (tree.pos includes context.unit.targetPos)
        val localTarget = stats exists includesTargetPos
        def typedStat(stat: Tree): Unit = {
          stat match {
            case imp @ Import(_, _) =>
              context = context.make(imp)
              addToContexts(imp, context)
            case _ =>              
              if (localTarget && !includesTargetPos(stat)) {
                // skip typechecking of statements in a sequence where some other statement includes
                // the targetposition
                addToContexts(stat, context)
              } else {
                val localContextCreator = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) {
                  this
                } else ContextCreator(context.make(stat, exprOwner))

                localContextCreator.contexted(stat)
                if (localContextCreator != this) this.treeContexts ++= localContextCreator.treeContexts 
              }
            }
        }

        stats map typedStat
      }
      
      def typedTemplate(templ: Template) = {
        val self = templ.self 

        if (self.name != nme.WILDCARD)
          context.scope enter self.symbol

        contextedStats(templ.body, templ.symbol)
      }
      
      def typedModuleDef(mdef: ModuleDef) = {
        val clazz     = mdef.symbol.moduleClass

        val impl1 = ContextCreator(context.make(mdef.impl, clazz, newScope)).typedTemplate(mdef.impl)
      }
      
      def typedClassDef(cdef: ClassDef) = {
        val clazz = cdef.symbol
        reenterTypeParams(cdef.tparams)
        val tparams1 = cdef.tparams map (typedTypeDef)
        val impl1 = ContextCreator(context.make(cdef.impl, clazz, newScope)).typedTemplate(cdef.impl)
      }
      
      def reenterTypeParams(tparams: List[TypeDef]): List[Symbol] =
        for (tparam <- tparams) yield {
          context.scope enter tparam.symbol
          tparam.symbol.deSkolemize
        }
      
      def reenterValueParams(vparamss: List[List[ValDef]]) {
        for (vparams <- vparamss)
          for (vparam <- vparams)
            context.scope enter vparam.symbol
      }
      
      def typedTypeDef(tdef: TypeDef): Unit =
        if (tdef.tparams.nonEmpty)
          ContextCreator(context.makeNewScope(tdef, tdef.symbol)).typedTypeDefImpl(tdef)
        else
          typedTypeDefImpl(tdef)

      private def typedTypeDefImpl(tdef: TypeDef) = {
        reenterTypeParams(tdef.tparams)
        val tparams1 = tdef.tparams map typedTypeDef

        val rhs1 = contexted(tdef.rhs)
      }
      
      def typedValDef(vdef: ValDef) = {
        val sym = vdef.symbol
        val valDefContextCreator = {
          val maybeConstrCtx =
            if ((sym.isParameter || sym.isEarlyInitialized) && sym.owner.isConstructor) context.makeConstructorContext
            else context
          ContextCreator(maybeConstrCtx.makeNewScope(vdef, sym))
        }
        valDefContextCreator.typedValDefImpl(vdef)
      }
          
      private def typedValDefImpl(vdef: ValDef) =
        ContextCreator(context).contexted(vdef.rhs)
      
      final def constrTyperIf(inConstr: Boolean) =
        if (inConstr) {
          ContextCreator(context.makeConstructorContext)
        } else this
    
      def defDefTyper(ddef: DefDef) = {
        val sym = ddef.symbol 
        val isConstrDefaultGetter = ddef.mods.hasDefault && sym.owner.isModuleClass &&
            nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
        ContextCreator(context.makeNewScope(ddef, sym)).constrTyperIf(isConstrDefaultGetter)
      }  
      
      def typedDefDef(ddef: DefDef) = {
        val meth = ddef.symbol

        reenterTypeParams(ddef.tparams)
        reenterValueParams(ddef.vparamss)

        val tparams1 = ddef.tparams map typedTypeDef
        val vparamss1 = ddef.vparamss map (_ map typedValDef)

        meth.annotations.map(_.completeInfo())

        var rhs1 =
          // if (ddef.name == nme.CONSTRUCTOR && !ddef.symbol.hasStaticFlag) { // need this to make it possible to generate static ctors
            ContextCreator(context).contexted(ddef.rhs)
          // } else {
          //  transformedOrTyped(ddef.rhs, EXPRmode, tpt1.tpe)
          // }

          // else if (meth.isMacro) {
            // typechecking macro bodies is sort of unconventional
            // that's why we employ our custom typing scheme orchestrated outside of the typer
            // transformedOr(ddef.rhs, typedMacroBody(this, ddef))
          // } 
      }
      
//      def typedQualifier(tree: Tree) =
//        contexted(tree)
//      
//      def typedSelectOrSuperCall(tree: Select) = tree match {
//        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
//          // the qualifier type of a supercall constructor is its first parent class
//          typedSelect(tree, typedSelectOrSuperQualifier(qual), nme.CONSTRUCTOR)
//        case Select(qual, name) =>
//          val qualTyped = typedQualifier(qual)
//          val qualStableOrError = (
//            if (qualTyped.isErrorTyped || !name.isTypeName || treeInfo.admitsTypeSelection(qualTyped))
//              qualTyped
//            else
//              UnstableTreeError(qualTyped)
//          )
//          val tree1 = name match {
//            case nme.withFilter if !settings.future => tryWithFilterAndFilter(tree, qualStableOrError)
//            case _              => typedSelect(tree, qualStableOrError, name)
//          }
//      }
        
      def typedBlock(block0: Block) = {
        //for (stat <- block0.stats) enterLabelDef(stat)

        val stats2 = contextedStats(block0.stats, context.owner)
        val expr1 = ContextCreator(context).contexted(block0.expr)
      }
      
      //return context resulted from tree processing
      def contexted(tree: Tree): Unit = {
        val sym: Symbol = tree.symbol
        val createdContext = tree match {
          case tree @ ModuleDef(_, _, impl) =>
            val moduleContext = context.makeNewScope(tree, sym)
            val newCreator = ContextCreator(moduleContext)
            newCreator.typedModuleDef(tree)
            treeContexts ++= newCreator.treeContexts 
            moduleContext

          case pdef: PackageDef => 
            val sym = tree.symbol
            val pdefCont = context.make(tree, sym.moduleClass, sym.info.decls)
            val newCreator = ContextCreator(pdefCont)
            newCreator.contextedStats(pdef.stats, NoSymbol)
            treeContexts ++= newCreator.treeContexts 
            pdefCont
          
          case tree: ClassDef => 
            val classContext = context.makeNewScope(tree, sym)
            val newCreator = ContextCreator(classContext)
            newCreator.typedClassDef(tree)
            treeContexts ++= newCreator.treeContexts 
            classContext
            
          case tree: TypeDef => typedTypeDef(tree)
            null
            
          case tree: ValDef => typedValDef(tree)
            null

          case tree: DefDef => defDefTyper(tree).typedDefDef(tree)
            null

          case tree: Block => typedBlock(tree)
            null
  
            
          case _ => null // TODO: add processing for all cases
        }
        addToContexts(tree, createdContext)
      }
    }
  }
}
