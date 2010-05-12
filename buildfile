require 'buildr/scala'

repositories.remote << 'http://repo1.maven.org/maven2/'
#COMMONS_IO = transitive('commons-io:commons-io:jar:1.4')
SPEC = transitive('org.scala-tools.testing:specs:jar:1.6.1')

define 'TemplateSyntaxAnalyzer' do
  project.version = '0.1'
  
  #compile.with COMMONS_IO
  compile.using :deprecation => true#, :other => '-Xplugin:target/TemplateSyntaxAnalyzer-0.1.jar'
  
  test.with SPEC
  package :jar
  
  task :version do
    puts "Scala: " + Scala.version
    puts "Project: " + project.version
  end
  
  task :buildTemplate => :package do
    system 'scalac -classpath target/classes -d target/classes -Xplugin:target/TemplateSyntaxAnalyzer-0.1.jar samples/*.scala samples/views/*' #-Xsource-reader jto.scala.compiler.readers.ScalhackReader'
  end
  
  task :scalap => :compile do
    puts "\n"
    system 'scalap -cp . views.pkg2.pkg3.Test'
  end
  
end