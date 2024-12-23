// Generated from D:/KT/MT/lab3/src/Clojure.g4 by ANTLR 4.13.1
package antlr.parser;
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link ClojureParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface ClojureVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link ClojureParser#program}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitProgram(ClojureParser.ProgramContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#surface_function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSurface_function(ClojureParser.Surface_functionContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction(ClojureParser.FunctionContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#function_body}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFunction_body(ClojureParser.Function_bodyContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#ns}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNs(ClojureParser.NsContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#defn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitDefn(ClojureParser.DefnContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#let}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLet(ClojureParser.LetContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#bindings}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBindings(ClojureParser.BindingsContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#binding}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBinding(ClojureParser.BindingContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#exprs}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExprs(ClojureParser.ExprsContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#arithmetic_function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArithmetic_function(ClojureParser.Arithmetic_functionContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#compare_function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCompare_function(ClojureParser.Compare_functionContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#general_function}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGeneral_function(ClojureParser.General_functionContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#args_ids}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArgs_ids(ClojureParser.Args_idsContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#form}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitForm(ClojureParser.FormContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#atom}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAtom(ClojureParser.AtomContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#operation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOperation(ClojureParser.OperationContext ctx);
	/**
	 * Visit a parse tree produced by {@link ClojureParser#comparison}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitComparison(ClojureParser.ComparisonContext ctx);
}