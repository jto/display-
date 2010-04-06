package jto.scala.compiler.plugins

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.util.{Position, NoPosition}
import scala.tools.nsc.ast.TreeGen
import scala.tools.nsc.ast.parser._
import scala.tools.nsc.symtab.Flags

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
        TemplateSyntaxAnalyzer.this.global.treeBrowsers.create().browse(unit.body) 
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
      
      def buildObject(className: String, parents: (List[Tree], List[List[Tree]]), renderDef: Tree): List[Tree] = {
        //Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position)
        val par = parents._1 //List(gen.scalaScalaObjectConstr.asInstanceOf[RefTree]) // SuperClasses  (Cf TreeGen.scala)
        val self = emptyValDef                                               // Current class ValDef
        val constrMods = NoMods                                              // Modifiers
        val vparamss =  List(List[ValDef]())                                 // Constructor parameters
        val argss = parents._2 //List(List[Tree]())                          // Parents class constructors params
        val body = List(renderDef)                                           // List[Tree] of members
        val superPos = NoPosition                                            // Position

        //extends jto.scala.template.Template
        // :: par
        
        val t =  atPos(NoPosition) (Template(par, self , constrMods, vparamss, argss, body, superPos))
        //List(ModuleDef(NoMods, newTermName(className), t)) //ModuleDef == object definition
        //TODO: add "case" modifier
        List(ClassDef(NoMods, newTermName(className).toTypeName, List(), t)) // => for Class
      }
            
      //Create "render" function definition 
      def buildRenderDef(body: List[Tree]): Tree = {
        val newmods = NoMods
        val name = newTermName("render")
        val tparams = List[TypeDef]()                                   //Template (generics) parameters ex: render[T](toto: T) = ...
        val vparamss = List(List[ValDef]())                             //method parameters TODO: set real parameters
        val restype = TypeTree()                                        //TypeTree, function's return type, will be infered

        //Declare an empty mutable String named "out"
        //TODO: "out" should be a StringBuffer 
        val outDef = ValDef(Modifiers(Flags.MUTABLE), newTermName("out"), TypeTree(), Literal(""))
        val outReturn = Ident("out")
        val completeBody: List[Tree] = outDef :: body
        val b = Block(completeBody,  outReturn)
        
        val rhs = atPos(NoPosition){ b }    //Method BODY
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
        val (parents,body) = template2Scala(sourcefile, TemplateParser.parse(content))
        createBaseTree(buildObject("Test", parents, buildRenderDef(body)))
      }
      
      /**
      * Convert Template AST To Scala AST
      */
      import scala.util.parsing.input.OffsetPosition
      import scala.collection.mutable.ListBuffer
      def template2Scala(sourcefile: BatchSourceFile, t: List[Expression]) = {
        var parents = (List[Tree](), List[List[Tree]]())
        val body = new ListBuffer[Tree]
        
        t foreach {
          _ match {
              case b @ ScalaValueBlock(e) => body += assign( Select(parser(sourcefile, b).block, newTermName("render")) )
              case b @ ScalaScriptBlock(e) => body ++= parser(sourcefile, b).blockStatSeq(new ListBuffer[Tree])
              case StaticValueBlock(e) => body += assign(Literal(e))
              //TODO: handle multiple extends
              case b @ ScalaExtends(e) => parents = parser(sourcefile, b).templateParents(false)
              case _ => throw new Exception("WTF am I doing here ?")
            } 
        }
        (parents, body toList)
      }
      
      def assign(node: Tree) = Assign(Ident("out"), Apply( Select(Ident("out"), newTermName("$plus")), List(node) ))
      
      /**
      * Return content parser
      */
      def parser(sourcefile: BatchSourceFile, b: Expression): Parser = {
        val p: scala.util.parsing.input.Position = b.pos
        //TODO: add real start (consider start tag length)
        val start = p.asInstanceOf[OffsetPosition].offset
        val end = start + b.expr.length
        /**
        * SourceFileFragment that match Position in scala block to Position in real file
        */
        import scala.tools.nsc.util.SourceFileFragment
        class TemplateFileFragment(sf: BatchSourceFile, start: Int, end: Int) extends SourceFileFragment(sf, start, end){
          //XXX: evil "; expected" workaround
          override val content = (b.expr + ";").toArray
          override def positionInUltimateSource(position: Position) = new scala.tools.nsc.util.OffsetPosition(sourcefile, position.point + start)
        }            
        val fragment = new TemplateFileFragment(sourcefile, start, end)
        new UnitParser(new CompilationUnit(fragment))
      }
      
    }
  }
}