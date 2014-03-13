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
            //val contextCreator = ContextCreator(tree, compUnit)
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
      def apply(tree: Tree, compUnit: CompilationUnit) = {
        val traverser = new {
          val unit = compUnit
          val defName = "test"
        } with ContextCreator
        traverser.newContext(tree)
      }
    }
    
    trait ContextCreator {
      val unit: CompilationUnit
      val defName: String
      
      var context = analyzer.rootContext(unit, EmptyTree, false)
      
      printScopeInfo
      
      def printScopeInfo = {
        println("==================")
        println(s"context: $context")
        println(s"scope: ${context.scope}")
        println("==================")
      }

      def newContext(tree: Tree): Unit = {
        tree match {
          case tree @ ModuleDef(_, _, impl) =>
            val sym = tree.symbol
            val clazz = sym.moduleClass

            // Self part
            context = context.makeNewScope(tree, clazz)
            //analyzer.newNamer(context).enterSelf(impl.self)

            // Body part
            context = context.makeNewScope(impl, clazz)

          case tree: PackageDef =>
            context = context.make(tree, tree.symbol.moduleClass, tree.symbol.info.decls)

          case tree: Template =>
            
          // Parsed already as ModuleDef children
          case _: TypeDef =>
            
          // Do Nothing
          case _: DefDef | _: ClassDef | _: ValDef =>

          case _ => // TODO: add processing for all cases
        }
      }
      
    }

  }
}
