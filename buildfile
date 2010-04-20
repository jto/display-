require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2/'
#COMMONS_IO = transitive('commons-io:commons-io:jar:1.4')
SPEC = transitive('org.scala-tools.testing:specs:jar:1.6.1')

define 'TemplateSyntaxAnalyzer' do
  project.version = '0.1'
  
  #compile.with COMMONS_IO
  compile.using :deprecation => true#, :other => '-Xplugin:../TreeBrowserPlugin/target/TreeBrowserPlugin-0.1.jar'
  
  test.with SPEC
  package :jar
  
  task :version do
    puts "Scala: " + Scala.version
    puts "Project: " + project.version
  end
  
  task :buildTemplate => :package do
    system 'scalac -cp target/classes src/main/views/Test.html.scala -Xplugin target/TemplateSyntaxAnalyzer-0.1.jar' #-Xsource-reader jto.scala.compiler.readers.ScalhackReader'
  end
  
  task :scalap => :compile do
    puts "\n"
    system 'scalap -cp . views.pkg2.pkg3.Test'
  end
  
end