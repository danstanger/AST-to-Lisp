import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Main {
    public static void main(String[] args) throws Exception {
	java.io.FileInputStream fis = new java.io.FileInputStream(args[0]);
        ANTLRInputStream input = new ANTLRInputStream(fis);
        PythonASTLexer lexer = new PythonASTLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        PythonASTParser parser = new PythonASTParser(tokens);
        CommonTree tree = parser.ast().getTree();
	String t = (tree.toStringTree()).replace(".","\\.");
        System.out.println(t);
    }
}

