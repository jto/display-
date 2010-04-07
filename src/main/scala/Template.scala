/***
 * Template
 * commons methods for templates and tags
 */
package jto.scala.template

trait Template extends ElvisConv{
  
  type T = Template
  
  implicit def any2Template(value: Any): DefaultTemplate = new DefaultTemplate(value)
  implicit def iterable2Template(value: Iterable[T]): IteratorTemplate = new IteratorTemplate(value)

  /***
  * Default template impl, used by tag in scala block when 
  * no other conversion is available
  */
  class DefaultTemplate(val value: Any) extends Template{
    override def render() = value.toString
  }

  /***
  * Automatic iteration template
  */
  class IteratorTemplate(val l: Iterable[T]) extends Template{
    override def render() = l map(_.render) mkString
  }

  def render(): String
}

/**
* Operators
*/
class Elvis(val me: Any){
  def ?:(f: Any) = if(f != null) f else me
}

trait ElvisConv{
  implicit def any2Elvis(a: Any): Elvis = new Elvis(a); 
}