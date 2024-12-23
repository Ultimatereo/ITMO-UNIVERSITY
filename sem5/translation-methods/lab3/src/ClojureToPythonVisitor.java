import antlr.parser.ClojureBaseVisitor;
import antlr.parser.ClojureParser;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.List;

class ClojureToPythonVisitor extends ClojureBaseVisitor<String> {
    @Override
    public String visitTerminal(TerminalNode node) {
        if (node.getSymbol().getType() == Token.EOF) {
            return "";
        }
        return node.getText();
    }

    @Override
    protected String defaultResult() {
        return "";
    }

    @Override
    protected String aggregateResult(String aggregate, String nextResult) {
        return aggregate + nextResult;
    }

    @Override
    public String visitSurface_function(ClojureParser.Surface_functionContext ctx) {
        String function = visit(ctx.children.get(0));
        return function + "\n";
    }

    @Override
    public String visitNs(ClojureParser.NsContext ctx) {
        return "";
    }

    @Override
    public String visitFunction(ClojureParser.FunctionContext ctx) {
        return visit(ctx.children.get(1));
    }

    @Override
    public String visitDefn(ClojureParser.DefnContext ctx) {
        String name = ctx.children.get(1).getText();
        String args_names = visit(ctx.children.get(3));
        String body = visit(ctx.children.getLast());

        return "def " + name + "(" + args_names + "):\n" +
                "\treturn " + body;
    }

    @Override
    public String visitArgs_ids(ClojureParser.Args_idsContext ctx) {
        ArrayList<String> args_ids = new ArrayList<>();
        for (ParseTree child : ctx.children) {
            args_ids.add(visit(child));
        }
        return String.join(",", args_ids);
    }

    @Override
    public String visitArithmetic_function(ClojureParser.Arithmetic_functionContext ctx) {
        String operation = ctx.children.get(0).getText();
        ArrayList<String> operands = getStringsExceptFirst(ctx.children);
        return "(" + String.join(operation, operands) + ")";
    }

    @Override
    public String visitCompare_function(ClojureParser.Compare_functionContext ctx) {
        return "(" + visit(ctx.children.get(1)) + ctx.children.get(0).getText() + visit(ctx.children.get(2)) + ")";
    }

    @Override
    public String visitGeneral_function(ClojureParser.General_functionContext ctx) {
        String func = ctx.children.get(0).getText();
        ArrayList<String> arguments = getStringsExceptFirst(ctx.children);
        return func + "(" + String.join(",", arguments) + ")";
    }

    private ArrayList<String> getStringsExceptFirst(List<ParseTree> ctx) {
        ArrayList<String> args_ids = new ArrayList<>();
        for (int i = 1; i < ctx.size(); i++) {
            args_ids.add(visit(ctx.get(i)));
        }
        return args_ids;
    }

    @Override
    public String visitLet(ClojureParser.LetContext ctx) {
        String bindings = visit(ctx.children.get(2));
        String exprs = visit(ctx.children.getLast());
        return bindings + exprs;
    }

    @Override
    public String visitBinding(ClojureParser.BindingContext ctx) {
        String var = ctx.children.get(0).getText();
        String value = visit(ctx.children.get(1));
        return var + "=" + value + "\n";
    }
}
