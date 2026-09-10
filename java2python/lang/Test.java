import org.antlr.runtime.*;
import org.antlr.runtime.tree.*;

public class Test {
    public static void main(String[] args) throws Exception {
	java.io.FileInputStream fis = new java.io.FileInputStream(args[0]);
        ANTLRInputStream input = new ANTLRInputStream(fis);
        JavaLexer lexer = new JavaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokens);
        CommonTree tree = parser.javaSource().getTree();
	String t = (tree.toStringTree()).replace(".","\\.");
        System.out.println(t);
    }
}

