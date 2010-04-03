package jto.scala.compiler.plugins

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.util.{Position, NoPosition}
import scala.tools.nsc.ast.TreeGen
import scala.tools.nsc.ast.parser._

import jto.scala.template.ast._

import java.io._

/**
* This plugin allow the Scala compiler to build any file as a scala class
* making them Templates to produce formatted output 
*/
class TemplateSyntaxAnalyzer(val global: Global) extends Plugin with Parsers{

  import global._

  val name = "templatesyntaxanalyzer"
  val description = "Build a scala AST from a template file"
  val components = List[PluginComponent](Component)

  /**
  * Build an AST from template file
  */
  private object Component extends PluginComponent{
    val global: TemplateSyntaxAnalyzer.this.global.type = TemplateSyntaxAnalyzer.this.global
    val runsAfter = List[String]()
    val phaseName = TemplateSyntaxAnalyzer.this.name
    override val runsRightAfter= Some("parser")
    def newPhase(_prev: Phase) = new TemplateSyntaxAnalyzerPhase(_prev)

    class TemplateSyntaxAnalyzerPhase(_prev: Phase) extends StdPhase(_prev){
      override def name = TemplateSyntaxAnalyzer.this.name
      
      import symtab.SymbolTable
      object gen  extends TreeGen{ 
        val global:SymbolTable = Component.global 
      }

      def apply(unit: CompilationUnit) {
        unit.body = loadTemplates
        //TemplateSyntaxAnalyzer.this.global.treeBrowsers.create().browse(unit.body)
      }
      
      // ==============================
      // REAL CODE STARTS HERE
      // ==============================
      def createBaseTree(tmplObj: List[Tree]): Tree = {
        val pkg = buildPkg
        atPos(NoPosition) { PackageDef(pkg.asInstanceOf[RefTree], tmplObj) }
      }

      def buildPkg(): Tree = {
        //TODO: read path and generate pkg name from it
        val names = List[String]("pkg2", "pkg3"); 
        val name = newTermName("views");
        val id: Tree = atPos(NoPosition)(Ident(name))
        (id /: names){ (i, n) =>  Select(i, newTermName(n)) } //Malbolge was too easy
      }
      
      def buildObject(className: String, renderDef: Tree): List[Tree] = {
        //Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position)
        val parents = List(gen.scalaScalaObjectConstr.asInstanceOf[RefTree]) // SuperClasses  (Cf TreeGen.scala)
        val self = emptyValDef                                               // Current class ValDef
        val constrMods = NoMods                                              // Modifiers
        val vparamss =  List(List[ValDef]())                                 // Constructor parameters
        val argss = List(List[Tree]())                                       // Parents class constructors params
        val body = List(renderDef)                                           // List[Tree] of members
        val superPos = NoPosition                                            // Position

        val t =  atPos(NoPosition) (Template(parents, self , constrMods, vparamss, argss, body, superPos))
        List(ModuleDef(NoMods, newTermName(className), t)) //ModuleDef == object definition
        //List(ClassDef(NoMods, newTermName(n).toTypeName, List(), t)) // => for Class
      }
            
      //Create "render" function definition 
      def buildRenderDef(body: Tree): Tree = {
        val newmods = NoMods
        val name = newTermName("render")
        val tparams = List[TypeDef]()                                   //Template (generics) parameters ex: render[T](toto: T) = ...
        val vparamss = List(List[ValDef]())                             //method parameters TODO: set real parameters
        val restype = TypeTree()                                        //TypeTree, function's return type, will be infered

        val rhs = atPos(NoPosition){ body }    //Method BODY
        DefDef(newmods, name, tparams, vparamss, restype, rhs)
      }
      
      //XXX: make the compiler load the templates files automatically
      def loadTemplates() = {
        val templateFolder = new File("src/main/views")
        
        if(!(templateFolder exists) || !(templateFolder isDirectory))
          throw new FileNotFoundException("views folder not found")
          
        val templates = templateFolder.listFiles().filter( f => !(f isHidden) && f.getName().endsWith(".html") )
        //TODO handle multiple files
        val ts = templates map createTemplateFromFile
        ts.head
      }
      
      
      import scala.tools.nsc.util.BatchSourceFile
      def createTemplateFromFile(f: File) = {
        val sourcefile = global.getSourceFile(f.getAbsolutePath).asInstanceOf[BatchSourceFile]
        val content = sourcefile.content mkString
        val body = template2Scala(sourcefile, TemplateParser.parse(content))
        createBaseTree(buildObject("Test", buildRenderDef(body)))
      }
      
      /**
      * Convert Template AST To Scala AST
      */
      import scala.util.parsing.input.OffsetPosition
      def template2Scala(sourcefile: BatchSourceFile, t: List[Expression]): Tree = {
        val node = t.head match {
          case b @ ScalaValueBlock(e) => parser(sourcefile, b).block
          
          //TODO
          case ScalaScriptBlock(e) => Literal("/* " + e + " */")
          case StaticValueBlock(e) => Literal(e)
          //TODO
          case ScalaExtends(e) => Literal("/*EXTENDS: " + e + " */")
          case _ => throw new Exception("WTF am I doing here ?")
        }
                
        if(t.tail isEmpty) node
        else Apply( Select(node, newTermName("$plus")), List(template2Scala(sourcefile, t.tail)) )
      }
      
      /**
      * Return content parser
      */
      def parser(sourcefile: BatchSourceFile, b: Expression): Parser = {
        val p: scala.util.parsing.input.Position = b.pos
        //TODO: get scala block start separator length
        val start = p.asInstanceOf[OffsetPosition].offset + 2
        val end = start + b.expr.length
        /**
        * SourceFileFragment that match Position in scala block to Position in real file
        */
        import scala.tools.nsc.util.SourceFileFragment
        class TemplateFileFragment(sf: BatchSourceFile, start: Int, end: Int) extends SourceFileFragment(sf, start, end){
          override def positionInUltimateSource(position: Position) = new scala.tools.nsc.util.OffsetPosition(sourcefile, position.point + start)
        }            
        val fragment = new TemplateFileFragment(sourcefile, start, end)
        new UnitParser(new CompilationUnit(fragment))
      }
      
    }
  }
}