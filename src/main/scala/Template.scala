/**
 * Template
 * commons methods for templates and tags
 */
package jto.scala.template
trait Template{
  implicit def anyToTemplate(value: Any): DefaultTemplate = new DefaultTemplate(value)
  def render(): String
}

/**
* Default template impl, used by tag in scala block when 
* no other conversion is available
*/
class DefaultTemplate(val value: Any) extends Template{
  override def render() = value.toString
}
