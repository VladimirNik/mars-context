package scala.mars.plugin

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }
import scala.tools.nsc.typechecker.Macros

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
            println("=================================================")
            println("================== PLUGIN WORK ==================")
            println("=================================================")
            println(s"phase name: ${prev.name}")
            println("unit: " + compUnit)
            println("=================================================")
            println("=============== Context Creator =================")  
            println("=================================================")
            val initialContext = analyzer.rootContext(compUnit, EmptyTree, false)
            
            import ContextFactory._
            
            ContextCreator(initialContext).contexted(tree)
            val contextInfo = applyContextInfo
            if (contextInfo.nonEmpty) {
              val (applyTree, resultedContext) = contextInfo.get
              println(s"\napplyTree: ${showRaw(applyTree)}\n")
              println(s"===> resulted context:\n $resultedContext\n")
              println(s"===> resulted scope:\n ${resultedContext.scope}\n")

              val untypedInjectTree = q"""println("Hello: " + a)"""
              val typechecker = global.analyzer.newTyper(resultedContext)
              val typedInjectTree = typechecker.typed(untypedInjectTree)
              println(s"\nresultedTree: ${showRaw(typedInjectTree)}\n")
              
              val unitTree = compUnit.body
              val transformedTree = RuntimeMacroInjector(unitTree, typedInjectTree).expandMacroTree
              println(s"\ntransformedTree: ${showRaw(transformedTree)}\n")
              println(s"\ntransformedShowCode: ${showCode(transformedTree)}\n")
              
              compUnit.body = transformedTree
            } else {
              println("\n===> tree is not found!!!\n")
            }
            //mode we should get from typer
            //global.analyzer.macroExpand(null, null, c.contextMode , null)
            println("=================================================")
          } else
            println("File " + fileName + " is not processed")
          } catch {
            case e: Exception =>
              e.printStackTrace()
              throw e
          }
      }
    }

    object ContextFactory {
      var applyContextInfo: Option[(Tree, Context)] = None
      
      def isRuntimeMacro(apply: Apply) = {
        val Apply(fun, args) = apply
        fun match {
          case Select(_, name) => name.toString() == "runtimeMacro"
          case _ => false
        }
      }
      
      object RuntimeMacroInjector {
        def apply(tree: Tree, macroTree: Tree) = new RuntimeMacroInjector(tree, macroTree)
      }

      class RuntimeMacroInjector(val body: Tree, val macroTree: Tree) extends Transformer {
        def expandMacroTree: Tree = transform(body)

        override def transform(tree: Tree) = {
          tree match {
            case apply : Apply if isRuntimeMacro(apply) => macroTree
            case _ => super.transform(tree)
          }
        }
      }

      object ContextCreator {
        def apply(context: Context) =
          new ContextCreator(context)
      }

      class ContextCreator(context0: Context) {
        var context = context0

        def contextedStats(stats: List[Tree], exprOwner: Symbol = NoSymbol): Unit = {
          val inBlock = exprOwner == context.owner
          def includesTargetPos(tree: Tree) =
            tree.pos.isRange && context.unit.exists && (tree.pos includes context.unit.targetPos)
          val localTarget = stats exists includesTargetPos
          def typedStat(stat: Tree): Unit = {
            stat match {
              case imp @ Import(_, _) =>
                context = context.make(imp)
              case _ =>
                if (localTarget && !includesTargetPos(stat)) {
                  // skip typechecking of statements in a sequence where some other statement includes
                  // the targetposition
                } else {
                  val localContextCreator = if (inBlock || (stat.isDef && !stat.isInstanceOf[LabelDef])) {
                    this
                  } else ContextCreator(context.make(stat, exprOwner))

                  localContextCreator.contexted(stat)
                }
            }
          }

          stats map typedStat
        }

        def contextedTemplate(templ: Template) = {
          val self = templ.self

          if (self.name != nme.WILDCARD)
            context.scope enter self.symbol

          contextedStats(templ.body, templ.symbol)
        }

        def contextedModuleDef(mdef: ModuleDef) = {
          val clazz = mdef.symbol.moduleClass

          ContextCreator(context.make(mdef.impl, clazz, newScope)).contextedTemplate(mdef.impl)
        }

        def contextedClassDef(cdef: ClassDef) = {
          val clazz = cdef.symbol
          reenterTypeParams(cdef.tparams)
          cdef.tparams map (contextedTypeDef)
          ContextCreator(context.make(cdef.impl, clazz, newScope)).contextedTemplate(cdef.impl)
        }

        def reenterTypeParams(tparams: List[TypeDef]): List[Symbol] =
          for (tparam <- tparams) yield {
            context.scope enter tparam.symbol
          }

        def reenterValueParams(vparamss: List[List[ValDef]]) {
          for (vparams <- vparamss)
            for (vparam <- vparams)
              context.scope enter vparam.symbol
        }

        def contextedTypeDef(tdef: TypeDef): Unit =
          if (tdef.tparams.nonEmpty)
            ContextCreator(context.makeNewScope(tdef, tdef.symbol)).contextedTypeDefImpl(tdef)
          else
            contextedTypeDefImpl(tdef)

        private def contextedTypeDefImpl(tdef: TypeDef) = {
          val tparams1 = tdef.tparams map contextedTypeDef
          contexted(tdef.rhs) // typedType ~ typed
        }

        def contextedValDef(vdef: ValDef) = {
          val sym = vdef.symbol
          val valDefContextCreator = {
            val maybeConstrCtx =
              if ((sym.isParameter || sym.isEarlyInitialized) && sym.owner.isConstructor) context.makeConstructorContext
              else context
            ContextCreator(maybeConstrCtx.makeNewScope(vdef, sym))
          }
          valDefContextCreator.contextedValDefImpl(vdef)
        }

        private def contextedValDefImpl(vdef: ValDef) =
          contexted(vdef.rhs)

        final def constrTyperIf(inConstr: Boolean) =
          if (inConstr) {
            ContextCreator(context.makeConstructorContext)
          } else this

        def defDefContextCreator(ddef: DefDef) = {
          val sym = ddef.symbol
          val isConstrDefaultGetter = ddef.mods.hasDefault && sym.owner.isModuleClass &&
            nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
          ContextCreator(context.makeNewScope(ddef, sym)).constrTyperIf(isConstrDefaultGetter)
        }

        def contextedDefDef(ddef: DefDef) = {
          val meth = ddef.symbol

          reenterTypeParams(ddef.tparams)
          reenterValueParams(ddef.vparamss)

          ddef.tparams map contextedTypeDef
          ddef.vparamss map (_ map contextedValDef)

          contexted(ddef.rhs)
        }

        def contextedBlock(block0: Block) = {
          //for (stat <- block0.stats) enterLabelDef(stat)

          contextedStats(block0.stats, context.owner)
          contexted(block0.expr)
        }

        //return context resulted from tree processing
        def contexted(tree: Tree): Context = {
          printBeforeTree(tree)
          
          val sym: Symbol = tree.symbol
          tree match {
            case tree @ ModuleDef(_, _, impl) =>
              val moduleContext = context.makeNewScope(tree, sym)
              val newCreator = ContextCreator(moduleContext)
              newCreator.contextedModuleDef(tree)

            case pdef @ PackageDef(pid, stats) =>
              val sym = tree.symbol
              contexted(pid)
              val pdefCont = context.make(tree, sym.moduleClass, sym.info.decls)
              ContextCreator(pdefCont).contextedStats(pdef.stats, NoSymbol)

            case tree: ClassDef =>
              val classContext = context.makeNewScope(tree, sym)
              ContextCreator(classContext).contextedClassDef(tree)

            case tree: TypeDef => contextedTypeDef(tree)

            case tree: ValDef => contextedValDef(tree)

            case tree: DefDef => defDefContextCreator(tree).contextedDefDef(tree)

            case tree: Block =>
              val blockContext = context.makeNewScope(tree, context.owner)
              ContextCreator(blockContext).contextedBlock(tree)

            case sup: Super => contexted(sup.qual)

            case select @ Select(qual, _) => contexted(qual)

            case apply @ Apply(fun, args) =>
              //TODO fix tree in context
              contexted(fun) // fix for args
              fun match {
                case Select(_, name) if name.toString() == "runtimeMacro" =>
                  applyContextInfo = Option(apply, context)
                case _ =>
              }

            case tree: This => // context shouldn't be changed

            case tree: Literal => // context shouldn't be changed

            case tree: Ident => // context shouldn't be changed

            case _ => // TODO: add processing for other trees
          }
          printScopeInfo(tree)
          context
        }

        def printScopeInfo(tree: Tree) = {
          println("=============================")
          println(s"show(tree): ${show(tree)}\n")
          println(s"showRaw(tree): ${showRaw(tree)}\n")
          println(s"context: $context\n")
          println(s"scope: ${context.scope}")
          println("=============================")
        }

        def printBeforeTree(tree: Tree) = {
          println(s"---------------------------")
          println(s"before typed: ${show(tree)}")
          println(s"---------------------------")
        }
      }
    }
  }
}
