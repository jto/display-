package jto.scala.template.example

import jto.scala.template._

trait ImplicitConversions{
  implicit def user2Template(u :User): UserTag = new UserTag(u)
}

class User(val name: String, val id: Long)
class UserTag(u: User) extends Template{
  override def render():String = "<p>(name=" + u.name + ", id=" + u.id + ")</p>"
}

class If(override val body: Unit => Template = {Unit => new DefaultTemplate("")}, test: Boolean) extends Template{
  def render(): String = if(test) body().render else ""
  /*
  new If(Unit => { println("rendering If"); new DefaultTemplate("Tag content") }, 1 < 2).render
  */
}