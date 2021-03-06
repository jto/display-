package jto.scala.template.ast

/**
* Template AST nodes classes
*/
import scala.util.parsing.input.Positional

sealed abstract class Expression(val expr: String) extends Positional
case class StaticValueBlock(override val expr: String) extends Expression(expr)
case class ScalaValueBlock(override val expr: String) extends Expression(expr)
case class ScalaScriptBlock(override val expr: String) extends Expression(expr)
case class ScalaExtends(override val expr: String) extends Expression(expr)
case class ScalaParams(override val expr: String) extends Expression(expr)

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
    
    //TODO: add i18n support with &{'key.key1.key2'}
    //TODO: add tags with body (iteration etc.)
    val BSTART = "${"
    val BEND   = "}"
    val SSTART = "%{"
    val SEND   = "}%"
    
    val EofCh = CharArrayReader.EofCh
    
    implicit def string2Parser(s: String): Parser[String] = accept(s.toList) ^^ { _.mkString }
    
    lazy val scalaBlock: Parser[Expression]  = BSTART ~> takeUntil(BEND) <~ BEND ^^ (ScalaValueBlock(_))
    lazy val scriptBlock: Parser[Expression] = SSTART ~> takeUntil(SEND) <~ SEND ^^ (ScalaScriptBlock(_))
    lazy val staticBlock: Parser[Expression] = takeUntilEnd(BSTART | SSTART | ext | params) ^^ (StaticValueBlock(_))
    lazy val ext: Parser[Expression] = "#{extends" ~> takeUntil("}") <~ "}" ^^ (ScalaExtends(_))
	  lazy val params: Parser[Expression] = "#{params" ~> takeUntil("}") <~ "}" ^^ { x: String => ScalaParams("(" + x + ")") }
		
    def tmpl = positioned(params | ext | scriptBlock | scalaBlock | staticBlock)+
     
    lazy val anyChar: Parser[Char] = chrExcept(EofCh)
    def chrExcept(cs: Char*): Parser[Char] = elem("chrExcept", ch => (ch != EofCh) && (cs forall (ch !=)))
    def takeUntil(cond: Parser[Any]): Parser[String] = takeUntil(cond, anyChar)
    def takeUntilEnd(cond: Parser[Any]): Parser[String] = takeUntilEnd(cond, anyChar)
		// TODO: make it fail if eof is reached
    def takeUntil(cond: Parser[Any], p: Parser[Char]): Parser[String] = rep(not(cond) ~> p) ^^ { _.mkString }
    // XXX: "not(EofCh) ~>" avoids infinite loops on rep(takeUntil(...))
    def takeUntilEnd(cond: Parser[Any], p: Parser[Char]): Parser[String] = (not(EofCh) ~> rep(not(cond) ~> p)) ^^ { _.mkString }
    
    def parse(content: String) = phrase(tmpl)(new CharSequenceReader(content))
}

object TemplateParser{
    def parse(content: String) = {
        val s = new TemplateScanner()
        val res = s.parse(content)
        res.getOrElse(List())
    }
}