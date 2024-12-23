// Generated from D:/KT/MT/lab3/src/Clojure.g4 by ANTLR 4.13.1
package antlr.parser;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link ClojureParser}.
 */
public interface ClojureListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link ClojureParser#program}.
	 * @param ctx the parse tree
	 */
	void enterProgram(ClojureParser.ProgramContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#program}.
	 * @param ctx the parse tree
	 */
	void exitProgram(ClojureParser.ProgramContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#surface_function}.
	 * @param ctx the parse tree
	 */
	void enterSurface_function(ClojureParser.Surface_functionContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#surface_function}.
	 * @param ctx the parse tree
	 */
	void exitSurface_function(ClojureParser.Surface_functionContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#function}.
	 * @param ctx the parse tree
	 */
	void enterFunction(ClojureParser.FunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#function}.
	 * @param ctx the parse tree
	 */
	void exitFunction(ClojureParser.FunctionContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#function_body}.
	 * @param ctx the parse tree
	 */
	void enterFunction_body(ClojureParser.Function_bodyContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#function_body}.
	 * @param ctx the parse tree
	 */
	void exitFunction_body(ClojureParser.Function_bodyContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#ns}.
	 * @param ctx the parse tree
	 */
	void enterNs(ClojureParser.NsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#ns}.
	 * @param ctx the parse tree
	 */
	void exitNs(ClojureParser.NsContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#defn}.
	 * @param ctx the parse tree
	 */
	void enterDefn(ClojureParser.DefnContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#defn}.
	 * @param ctx the parse tree
	 */
	void exitDefn(ClojureParser.DefnContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#let}.
	 * @param ctx the parse tree
	 */
	void enterLet(ClojureParser.LetContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#let}.
	 * @param ctx the parse tree
	 */
	void exitLet(ClojureParser.LetContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#bindings}.
	 * @param ctx the parse tree
	 */
	void enterBindings(ClojureParser.BindingsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#bindings}.
	 * @param ctx the parse tree
	 */
	void exitBindings(ClojureParser.BindingsContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#binding}.
	 * @param ctx the parse tree
	 */
	void enterBinding(ClojureParser.BindingContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#binding}.
	 * @param ctx the parse tree
	 */
	void exitBinding(ClojureParser.BindingContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#exprs}.
	 * @param ctx the parse tree
	 */
	void enterExprs(ClojureParser.ExprsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#exprs}.
	 * @param ctx the parse tree
	 */
	void exitExprs(ClojureParser.ExprsContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#arithmetic_function}.
	 * @param ctx the parse tree
	 */
	void enterArithmetic_function(ClojureParser.Arithmetic_functionContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#arithmetic_function}.
	 * @param ctx the parse tree
	 */
	void exitArithmetic_function(ClojureParser.Arithmetic_functionContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#compare_function}.
	 * @param ctx the parse tree
	 */
	void enterCompare_function(ClojureParser.Compare_functionContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#compare_function}.
	 * @param ctx the parse tree
	 */
	void exitCompare_function(ClojureParser.Compare_functionContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#general_function}.
	 * @param ctx the parse tree
	 */
	void enterGeneral_function(ClojureParser.General_functionContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#general_function}.
	 * @param ctx the parse tree
	 */
	void exitGeneral_function(ClojureParser.General_functionContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#args_ids}.
	 * @param ctx the parse tree
	 */
	void enterArgs_ids(ClojureParser.Args_idsContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#args_ids}.
	 * @param ctx the parse tree
	 */
	void exitArgs_ids(ClojureParser.Args_idsContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#form}.
	 * @param ctx the parse tree
	 */
	void enterForm(ClojureParser.FormContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#form}.
	 * @param ctx the parse tree
	 */
	void exitForm(ClojureParser.FormContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#atom}.
	 * @param ctx the parse tree
	 */
	void enterAtom(ClojureParser.AtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#atom}.
	 * @param ctx the parse tree
	 */
	void exitAtom(ClojureParser.AtomContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#operation}.
	 * @param ctx the parse tree
	 */
	void enterOperation(ClojureParser.OperationContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#operation}.
	 * @param ctx the parse tree
	 */
	void exitOperation(ClojureParser.OperationContext ctx);
	/**
	 * Enter a parse tree produced by {@link ClojureParser#comparison}.
	 * @param ctx the parse tree
	 */
	void enterComparison(ClojureParser.ComparisonContext ctx);
	/**
	 * Exit a parse tree produced by {@link ClojureParser#comparison}.
	 * @param ctx the parse tree
	 */
	void exitComparison(ClojureParser.ComparisonContext ctx);
}