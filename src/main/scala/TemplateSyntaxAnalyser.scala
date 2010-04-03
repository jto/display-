package jto.scala.compiler.plugins

import scala.tools.nsc._
import scala.tools.nsc.plugins._
import scala.tools.nsc.util.{Position, NoPosition}
import scala.tools.nsc.ast.TreeGen
import scala.tools.nsc.ast.parser._

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
      import scala.tools.nsc.util.SourceFileFragment
      import scala.util.parsing.input.OffsetPosition
      def template2Scala(sourcefile: BatchSourceFile, t: List[Expression]): Tree = {
        val node = t.head match {
          case b @ ScalaValueBlock(e) => {
            val p: scala.util.parsing.input.Position = b.pos
            //TODO: get scala block start separator length
            val start = p.asInstanceOf[OffsetPosition].offset + 2
            val end = start + e.length()

            /**
            * SourceFileFragment that match Position in scala block to Position in real file
            */
            import scala.tools.nsc.util.FakePos
            class TemplateFileFragment(sf: BatchSourceFile, start: Int, end: Int) extends SourceFileFragment(sf, start, end){
              override def positionInUltimateSource(position: Position) = new scala.tools.nsc.util.OffsetPosition(sourcefile, position.point + start)
            }            
            
            val fragment = new TemplateFileFragment(sourcefile, start, end)
            new UnitParser(new CompilationUnit(fragment)).block()
          }
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
      
    }
  }
}


/**
* Template AST classes`
* TODO: use Real classes from scala compiler, and invoque Scala Parser when Scala blocks are found
*/
import scala.util.parsing.input.Positional

sealed abstract class Expression extends Positional
case class ScalaValueBlock(expr: String) extends Expression
case class StaticValueBlock(expr: String) extends Expression
case class ScalaScriptBlock(expr: String) extends Expression
case class ScalaScriptBlockWithBody(expr: ScalaScriptBlock, body: List[Expression]) extends Expression
case class ScalaExtends(expr: String) extends Expression

// Cf: http://github.com/paulp/scala-lang-combinators/blob/master/src/MyScanners.scala
import scala.util.parsing.combinator.{ PackratParsers, ImplicitConversions }
import scala.util.parsing.syntax._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input.{ CharArrayReader, CharSequenceReader }

trait CharParsers extends PackratParsers {
  type Elem = Char
  type Token = Expression
}

class TemplateScanner extends CharParsers with ImplicitConversions{
    
    val BSTART = "{{"
    val BEND   = "}}"
    
    val SSTART   = "{%"
    val SEND   = "%}"
    
    val EofCh = CharArrayReader.EofCh
    
    implicit def string2Parser(s: String): Parser[String] = accept(s.toList) ^^ { _.mkString }
    
    lazy val scalaBlock: Parser[Expression]  = BSTART ~> takeUntil(BEND) <~ BEND ^^ (ScalaValueBlock(_))
    lazy val scriptBlock: Parser[Expression] = SSTART ~> takeUntil(SEND) <~ SEND ^^ (ScalaScriptBlock(_))
    lazy val staticBlock: Parser[Expression] = takeUntilEnd(BSTART | SSTART | ext) ^^ (StaticValueBlock(_))
    lazy val ext: Parser[Expression] = "#{extends:" ~> takeUntil("}") <~ "}" ^^ (ScalaExtends(_))
    
    def tmpl = positioned(ext | scriptBlock | scalaBlock | staticBlock)+
     
    lazy val anyChar: Parser[Char] = chrExcept(EofCh)
    def chrExcept(cs: Char*): Parser[Char] = elem("chrExcept", ch => (ch != EofCh) && (cs forall (ch !=)))
    def takeUntil(cond: Parser[Any]): Parser[String] = takeUntil(cond, anyChar)
    def takeUntilEnd(cond: Parser[Any]): Parser[String] = takeUntilEnd(cond, anyChar)
    def takeUntil(cond: Parser[Any], p: Parser[Char]): Parser[String] = rep(not(cond) ~> p) ^^ { _.mkString }
    //"not(EofCh) ~>" avoids infinite loops on rep(takeUntil(...))
    def takeUntilEnd(cond: Parser[Any], p: Parser[Char]): Parser[String] = (not(EofCh) ~> rep(not(cond) ~> p)) ^^ { _.mkString }
    
    def parse(content: String) = phrase(tmpl)(new CharSequenceReader(content))
}

object TemplateParser{
    def parse(content: String) = {
        val s = new TemplateScanner()
        val res = s.parse(content)
        println(res)
        res.getOrElse(List())
    }
}
