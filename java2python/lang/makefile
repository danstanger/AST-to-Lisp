all: Test.class

JavaParser.java: Java.g
	antlr3 Java.g
Test.class : JavaParser.java
	javac -d classes -g -cp .:./classes:/home/dxs/antlr3/antlr-3.5.3-complete.jar *.java
	java -cp .:./classes:/home/dxs/antlr3/antlr-3.5.3-complete.jar Test HelloWorld.java
tar:	Java.tar.gz
	tar -czvf Java.tar.gz Test.java J*.g makefile

clean:
	rm -rf JavaParser.java JavaLexer.java Java.tokens ./classes/*.class
