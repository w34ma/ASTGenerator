package ASTGenerator;

import java.io.*;
import java.util.*;
import javax.swing.*;

import org.antlr.v4.runtime.*;
import org.antlr.v4.tool.Grammar;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.gui.TreeViewer;

public class ASTGenerator {

    private static void print(ParseTree node) {
        if (node.getChildCount() == 0) {
            System.out.println("Leaf node: " + node.getText());
        } else {
            System.out.println("Non-leaf node: type " + node.getClass().getName());
            for (int i = 0; i < node.getChildCount(); i++) {
                System.out.println("Child " + i);
                print(node.getChild(i));
            }
        }
    }

    public static String parse(String fileName, String combinedGrammarFileName, String startRule) throws IOException {
        /*
        final Grammar g = Grammar.load(combinedGrammarFileName);
        LexerInterpreter lexEngine = g.createLexerInterpreter(new ANTLRFileStream(fileName));
        CommonTokenStream tokens = new CommonTokenStream(lexEngine);
        ParserInterpreter parser = g.createParserInterpreter(tokens);
        ParseTree t = parser.parse(g.getRule(startRule).index);
        */
        CommonLexer lexer = new CommonLexer(new ANTLRFileStream(fileName));
        BufferedTokenStream tokens = new BufferedTokenStream(lexer);
        CommonParser parser = new CommonParser(tokens);
        ParserRuleContext tree = parser.methodBody();

        print(tree);

        JFrame frame = new JFrame("Antlr4 Java AST");
        JPanel panel = new JPanel();
        TreeViewer viewer = new TreeViewer(Arrays.asList(parser.getRuleNames()),tree);
        panel.add(viewer);
        viewer.setScale(0.9);
        frame.add(panel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(1000, 1000);
        frame.setVisible(true);

        return tree.toStringTree(parser);
    }


    public static void main(String[] args) throws IOException, ClassNotFoundException {

        /*
        String ast = parse("/Users/Vivi/Downloads/src/test/test.txt",
                "/Users/Vivi/Downloads/ASTGenerator/src/main/antlr4/ASTGenerator/CPP14.g4",
                "functionbody");
        */
        String ast1 = parse("/Users/Vivi/Downloads/src/test/test.txt",
                "/Users/Vivi/Downloads/ASTGenerator/src/main/antlr4/ASTGenerator/Common.g4",
                "methodBody");

        // System.out.println(ast);
    }
}
