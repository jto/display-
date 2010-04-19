package jto.scala.template.example

/**
* TODO: users2template AND UsersTag should be generated automatically
*/

trait ImplicitConversions{
  implicit def user2Template(u :User): UserTag = new UserTag(u)
  implicit def users2Template(u :Iterable[User]): UsersTag = new UsersTag(u)
}

class User(val name: String, val id: Long)
	
import jto.scala.template.Template
class UserTag(u: User) extends Template
{
  override def render():String = "<p>(name=" + u.name + ", id=" + u.id + ")</p>"
}

class UsersTag(u: Iterable[User]) extends Template with ImplicitConversions
{
  override def render():String = u map (_.render) mkString
}
