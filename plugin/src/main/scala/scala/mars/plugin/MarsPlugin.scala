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

    override val runsAfter = List("typer")

    val phaseName = "marsPhase"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {
        try {
          //process only scala files
          val fileName = unit.source.file.name
          if (fileName.endsWith(".scala")) {
            val tree = unit.body
            MarsTransformer(expandName)
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

    object MarsTransformer {
      def apply(defName: String) =
        new MarsTransformer(defName)
    }

    class MarsTransformer(defName: String) extends Transformer {
      override def transform(tree: Tree): Tree = {
        tree match {
          //TODO expand...
          case vdDef: ValDef if vdDef.name == defName => {
            vdDef
          }
          case _ =>
            super.transform(tree)
        }
      }
    }

  }
}
