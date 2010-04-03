package jto.scala.template.exemple

trait ImplicitConversions{
  implicit def userToTemplate(u :User): UserTag = new UserTag(u)
}

class User(val name: String, val id: Long)

import jto.scala.template.Template
class UserTag(u: User) extends Template
{
  override def render():String = "<p>" + u.name + " (" + u.id + ")</p>"
}