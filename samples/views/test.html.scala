object f{val l = """
#{extends jto.scala.template.Template with jto.scala.template.example.ImplicitConversions}
#{params u: jto.scala.template.example.User = new jto.scala.template.example.User("default user", 0)}
%{ 
 import jto.scala.template.example.User
 import jto.scala.template.Template 
}%
<!DOCTYPE html>
<html>
	<head>
		<meta http-equiv="Content-type" content="text/html; charset=utf-8">
		<title>Test template scala</title>
	</head>
	%{
		val u1 = new User("toto1", 1)
		val u2 = new User("toto2", 2)
		val u3 = new User("toto3", 3)
		val u4 = new User("toto4", 4)
		val u5 = new User("toto4", 5)
		//=====
		class Tutu(val v: String)
		val t  = new Tutu("blop")
		val t2 = null
		//=====
		val users = List(u1, u2, u3, u4, u5)
	}%
	<body>
	  
		<div>
			<h1>Param</h1>
			<p>${u}</p>
		</div>
		<div
			<h1>Default</h1>
			<p>${t}</p>
		</div>	
		<div>
			<h1>Elvis Operator</h1>
			<p>${t2 ?: "ARRRG"}</p>
		</div>
		<div>
			<h1>UserTag</h1>
			${u1}
		</div>
		<div>
			<h1>Users tag</h1>
			${users}
		</div>
		<div>
		  #{If(false)}
		    <p>This should NOT be displayed</p>
		  #{/If}
		</div>
	</body>
</html>
"""}