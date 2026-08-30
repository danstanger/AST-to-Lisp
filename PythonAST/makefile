all: Main.class

PythonASTParser.java: PythonAST.g
	antlr3 PythonAST.g
Main.class : PythonASTParser.java *.java
	javac -d classes -g -cp .:./classes:/home/dxs/antlr3/antlr-3.5.3-complete.jar *.java
	java -cp .:./classes:/home/dxs/antlr3/antlr-3.5.3-complete.jar Main for.ast
tar:	PythonAST.tar.gz
	tar -czvf PythonAST.tar.gz Main.java J*.g makefile

clean:
	rm -rf PythonASTParser.java PythonASTLexer.java PythonAST.tokens ./classes/*.class
