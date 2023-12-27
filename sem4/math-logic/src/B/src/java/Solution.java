import java.util.*;

public class Solution {
    static String[] axioms = {
            parseExpression("a->b->a"),
            parseExpression("(a->b)->(a->b->c)->(a->c)"),
            parseExpression("a->b->a&b"),
            parseExpression("a&b->a"),
            parseExpression("a&b->b"),
            parseExpression("a->a|b"),
            parseExpression("b->a|b"),
            parseExpression("(a->c)->(b->c)->(a|b->c)"),
            parseExpression("(a->b)->(a->!b)->(!a)"),
            parseExpression("!!a->a")
    };

    static ArrayList<ArrayList<ProofItem>> knownRecipes;
    public static void main(String[] args) {
        ArrayList<ContextAndStatement> statements = new ArrayList<>();
        ArrayList<String> rawStatements = new ArrayList<>();
        Scanner sc = new Scanner(System.in);
        while (sc.hasNext()) {
            String _line = sc.nextLine();
            if (_line.equals("STOP")) {
                break;
            }
            StringBuilder line = new StringBuilder();
            for (int i = 0; i < _line.length(); i++) {
                char x = _line.charAt(i);
                if (!Character.isWhitespace(x)) {
                    line.append(x);
                }
            }
            statements.add(new ContextAndStatement(line.toString()));
            rawStatements.add(line.toString());
        }
        sc.close();
        getReasons(statements);

        for (int i = 0; i < statements.size(); i++) {
            System.out.print("[" + (i + 1) + "] ");
            System.out.print(rawStatements.get(i) + " ");
            System.out.println(statements.get(i).reason);
        }

//        knownRecipes = new ArrayList<>();
//        for (int i = 0; i < statements.size(); i++) {
//            knownRecipes.add(new ArrayList<>());
//        }

//        ArrayList<ProofItem> proof = getProof(statements, statements.size() - 1);
//
//        System.out.println(rawStatements.get(rawStatements.size() - 1));
//        for (ProofItem pi : proof) {
//            System.out.println(pi.rawExpression);
//        }

    }

    private static ArrayList<ProofItem> getProof(
            ArrayList<ContextAndStatement> statements,
            int index) {
        if (knownRecipes.get(index).size() != 0) {
            return new ArrayList<>(knownRecipes.get(index));
        }
        ContextAndStatement statement = statements.get(index);
        String reason = statement.reason;
        if (reason.charAt(1) == 'D') {
            int start = reason.indexOf('.');
            int end = reason.length() - 1;
            int number = Integer.parseInt(reason.substring(start + 2, end)) - 1;
            knownRecipes.set(index, constructDeduction(index, number, statements));
        } else if (reason.charAt(1) == 'M') {
            ArrayList<Integer> numbers = new ArrayList<>();
            int i = 0;
            while (i < reason.length()) {
                if (!Character.isDigit(reason.charAt(i))) {
                    i += 1;
                } else {
                    StringBuilder num = new StringBuilder();
                    while (Character.isDigit(reason.charAt(i))) {
                        num.append(reason.charAt(i));
                        i += 1;
                    }
                    numbers.add(Integer.parseInt(num.toString()) - 1);
                }
            }
            ArrayList<ProofItem> proof1 = getProof(statements, numbers.get(0));
            ArrayList<ProofItem> proof2 = getProof(statements, numbers.get(1));
            for (ProofItem proofItem : proof2) {
                reason = proofItem.reason;
                if (reason.charAt(1) == 'M') {
                    numbers = new ArrayList<>();
                    i = 0;
                    while (i < reason.length()) {
                        if (!Character.isDigit(reason.charAt(i))) {
                            i += 1;
                        } else {
                            StringBuilder num = new StringBuilder();
                            while (Character.isDigit(reason.charAt(i))) {
                                num.append(reason.charAt(i));
                                i += 1;
                            }
                            numbers.add(Integer.parseInt(num.toString()) - 1);
                        }
                    }
                    proofItem.reason = "[M.P. " + (numbers.get(0) + proof1.size()) + ", " +
                        (numbers.get(1) + proof1.size()) + "]";
                }
            }
            ArrayList<ProofItem> proof = new ArrayList<>();
            proof.addAll(proof1);
            proof.addAll(proof2);
            proof.add(new ProofItem(
                    normalize(statement.rawExpression),
                    "[M.P. " + (proof1.size()) + ", " + (proof.size()) + "]"
            ));
            knownRecipes.set(index, proof);
        } else {
            ArrayList<ProofItem> proof = new ArrayList<>();
            proof.add(new ProofItem(
                    normalize(statement.rawExpression),
                    "[Hyp. or Ax.]"
            ));
            knownRecipes.set(index, proof);
        }
        return new ArrayList<>(knownRecipes.get(index));
    }

    private static ArrayList<ProofItem> constructDeduction(
            int index_new, int index_old,
            ArrayList<ContextAndStatement> statements) {
        ContextAndStatement statement = statements.get(index_new);
        ContextAndStatement prevStatement = statements.get(index_old);
        ArrayList<String> statementNew = statement.enhancedContextList;
        ArrayList<String> statementOld = prevStatement.enhancedContextList;
        ArrayList<ProofItem> curProof = getProof(statements, index_old);
        int i = statementNew.size() - 2;
        int j = statementOld.size() - 2;
        while (i >= 0 && j >= 0) {
            if (statementNew.get(i).equals(statementOld.get(j))) {
                i -= 1;
                j -= 1;
            } else {
                break;
            }
        }
        for (int jj = 0; jj <= j; jj++) {
            String hyp = statementOld.get(jj);
            String rest = String.join("->", statementOld.subList(jj + 1, statementOld.size()));
            addHyp(hyp, rest, curProof);
        }
        for (int ii = i; ii >= 0; ii--) {
            String hyp = statementNew.get(ii);
            curProof = deleteHyp(hyp, curProof);
        }
        return curProof;
    }

    private static ArrayList<ProofItem> deleteHyp(
            String hyp, ArrayList<ProofItem> curProof) {
        ArrayList<ProofItem> newProof = new ArrayList<>();
        int[] oldToNew = new int[curProof.size()];
        for (int i = 0; i < curProof.size(); i++) {
            ProofItem st = curProof.get(i);
            String dn = st.rawExpression;
            int n = newProof.size();
            if (dn.equals(hyp)) {
                newProof.add(new ProofItem(
                        hyp + "->(" + hyp + "->" + hyp + ")",
                        "[Ax. sch. 1]"
                ));
                newProof.add(new ProofItem(
                        "(" + hyp + "->(" + hyp + "->" + hyp + "))->" +
                                "(" + hyp + "->(" + hyp + "->" + hyp + ")->" + hyp + ")->" +
                                "(" + hyp + "->" + hyp + ")",
                        "[Ax. sch. 2]"
                ));
                newProof.add(new ProofItem(
                                "(" + hyp + "->(" + hyp + "->" + hyp + ")->" + hyp + ")->" +
                                "(" + hyp + "->" + hyp + ")",
                        "[M.P. " + (n + 1) + ", " + (n + 2) + "]"
                ));
                newProof.add(new ProofItem(
                        hyp + "->(" + hyp + "->" + hyp + ")->" + hyp,
                        "[Ax. sch. 1]"
                ));
                newProof.add(new ProofItem(
                        hyp + "->" + hyp,
                        "[M.P. " + (n + 4) + ", " + (n + 3) + "]"
                ));
                oldToNew[i] = n + 5;
            } else if (st.reason.charAt(1) == 'M') {
                String reason = st.reason;
                int j = Integer.parseInt(
                        reason.substring(
                                reason.indexOf(' ') + 1,
                                reason.indexOf(',')
                        )
                ) - 1;
                int k = Integer.parseInt(
                        reason.substring(
                                reason.indexOf(',') + 2,
                                reason.length() - 1
                        )
                ) - 1;
                String dj = curProof.get(j).rawExpression;
                newProof.add(new ProofItem(
                        "(" + hyp + "->" + dj + ")->" +
                                "(" + hyp + "->" + dj + "->" + dn + ")->" +
                                "(" + hyp + "->" + dn + ")",
                        "[Ax. sch. 2]"
                ));
                newProof.add(new ProofItem(
                                "(" + hyp + "->" + dj + "->" + dn + ")->" +
                                "(" + hyp + "->" + dn + ")",
                        "[M.P. " + (oldToNew[j]) + ", " + (n + 1) + "]"
                ));
                newProof.add(new ProofItem(
                                hyp + "->" + dn,
                        "[M.P. " + (oldToNew[k]) + ", " + (n + 2) + "]"
                ));
                oldToNew[i] = n + 3;
            } else {
                newProof.add(new ProofItem(
                        dn + "->" + hyp + "->" + dn, "[Ax. sch. 1]"
                ));
                newProof.add(new ProofItem(
                        dn, "[Ax. or Hyp.]"
                ));
                newProof.add(new ProofItem(
                        hyp +  "->" + dn, "[M.P. " + (n + 2) + ", " + (n + 1) + "]"
                ));
                oldToNew[i] = n + 3;
            }
        }
        for (ProofItem proofItem : newProof) {
            proofItem.rawExpression = normalize(proofItem.rawExpression);
        }
        return newProof;
    }

    private static void addHyp(String hyp, String rest,
                               ArrayList<ProofItem> curProof) {
        int n = curProof.size();
        curProof.add(new ProofItem(hyp, "[Hyp]"));
        curProof.add(new ProofItem(rest, "[M.P. " + (n + 1) + ", " + (n) + "]"));
    }

    private static String normalize(String raw_expression) {
        return normalizeExpr(parseExpression(raw_expression));
    }
    private static void getReasons(ArrayList<ContextAndStatement> statements) {
        Map<Map<String, Integer>, Map<String, ArrayList<Integer>>> statementsSet = new HashMap<>();
        Map<Map<String, Integer>, Map<String, ArrayList<Pair<Integer, String>>>> statementsSecondSet = new HashMap<>();
        Map<Pair<Map<String, Integer>, String>, Integer> mapForDeduction = new HashMap<>();
        for (int i = 0; i < statements.size(); i++) {
            ContextAndStatement statement = statements.get(i);
            if (!checkForAxiom(statement)) {
                if (!checkForHyp(statement)) {
                    if (!checkForMP(statements, i, statementsSet, statementsSecondSet)) {
                        checkForDeduction(statements, i, mapForDeduction);
                    }
                }
            }
            mapForDeduction.putIfAbsent(statement.enhancedContext, i);
            statementsSet.putIfAbsent(statement.contextCount, new HashMap<>());
            statementsSecondSet.putIfAbsent(statement.contextCount, new HashMap<>());
            Map<String, ArrayList<Integer>> statementSet = statementsSet.get(statement.contextCount);
            Map<String, ArrayList<Pair<Integer, String>>> statementSecondSet = statementsSecondSet.get(statement.contextCount);
            String stC = statement.statement;
            statementSet.putIfAbsent(stC, new ArrayList<>());
            statementSet.get(stC).add(i);
            if (stC.charAt(0) != '(') {
                continue;
            }
            OperationAndOperands oao = new OperationAndOperands(stC);
            if (oao.operation.equals("->")) {
                statementSecondSet.putIfAbsent(oao.operand2, new ArrayList<>());
                statementSecondSet.get(oao.operand2).add(new Pair<>(i, oao.operand1));
            }

        }
    }

    private static boolean checkForDeduction(
            ArrayList<ContextAndStatement> statements,
            int index,
            Map<Pair<Map<String, Integer>, String>, Integer> mapForDeduction) {
        ContextAndStatement statement = statements.get(index);
        if (mapForDeduction.containsKey(statement.enhancedContext)) {
            statement.reason = "[Ded. " + (mapForDeduction.get(statement.enhancedContext) + 1) + "]";
            return true;
        }
        return false;
    }

    private static boolean checkForMP(
            ArrayList<ContextAndStatement> statements, int index,
            Map<Map<String, Integer>, Map<String, ArrayList<Integer>>> statementsSet,
            Map<Map<String, Integer>, Map<String, ArrayList<Pair<Integer, String>>>> statementsSecondSet) {
        String st = statements.get(index).statement;
        Map<String, Integer> context = statements.get(index).contextCount;
        if (statementsSet.containsKey(context)) {
            Map<String, ArrayList<Integer>> statementSet = statementsSet.get(context);
            Map<String, ArrayList<Pair<Integer, String>>> statementSecondSet = statementsSecondSet.get(context);
            if (statementSecondSet.containsKey(st)) {
                ArrayList<Pair<Integer, String>> arr = statementSecondSet.get(st);
                for (Pair<Integer, String> pair : arr) {
                    int i = pair.first;
                    String operand = pair.second;
                    if (statementSet.containsKey(operand)) {
                        int num = statementSet.get(operand).get(0);
                        statements.get(index).reason = "[M.P. " + (num + 1) + ", " + (i + 1) + "]";
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static boolean checkForHyp(ContextAndStatement line) {
        String statement = line.statement;
        Map<String, Integer> context = line.context;
        if (context.containsKey(statement)) {
            line.reason = "[Hyp. " + context.get(statement) + "]";
            return true;
        }
        return false;
    }

    private static boolean checkForAxiom(ContextAndStatement line) {
        String statement = line.statement;
        if (statement.charAt(0) != '(') {
            return false;
        }
        for (int i = 0; i < axioms.length; i++) {
            String axiom = axioms[i];
            if (equalsStatementToAxiom(statement, axiom)) {
                line.reason = "[Ax. sch. " + (i + 1) + "]";
                return true;
            }
        }
        return false;
    }

    private static boolean equalsStatementToAxiom(String statement, String axiom) {
        return equalsStatementToAxiom(statement, axiom, null);
    }

    private static boolean equalsStatementToAxiom(String statement, String axiom, Map<String, String> elements) {
        if (elements == null) {
            elements = new HashMap<>();
        }
        if (axiom.charAt(0) != '(') {
            if (elements.containsKey(axiom)) {
                return statement.equals(elements.get(axiom));
            }
            elements.put(axiom, statement);
            return true;
        }
        if (statement.charAt(0) == '(' && axiom.charAt(0) == '(') {
            OperationAndOperands axOAO = new OperationAndOperands(axiom);
            String[] axOperands = {axOAO.operand1, axOAO.operand2};
            OperationAndOperands oao = new OperationAndOperands(statement);
            String[] operands = {oao.operand1, oao.operand2};
            if (!axOAO.operation.equals(oao.operation)) {
                return false;
            }
            for (int i = 0; i < axOAO.numb; i++) {
                String axOperand = axOperands[i];
                String operand = operands[i];
                boolean b = equalsStatementToAxiom(operand, axOperand, elements);
                if (!b) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    public static class ContextAndStatement {
        Map<String, Integer> context;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ContextAndStatement statement1 = (ContextAndStatement) o;
            return Objects.equals(context, statement1.context) && Objects.equals(statement, statement1.statement) && Objects.equals(reason, statement1.reason) && Objects.equals(enhancedContext, statement1.enhancedContext) && Objects.equals(contextCount, statement1.contextCount) && Objects.equals(rawExpression, statement1.rawExpression) && Objects.equals(rawContextCount, statement1.rawContextCount) && Objects.equals(enhancedContextList, statement1.enhancedContextList);
        }

        @Override
        public int hashCode() {
            return Objects.hash(context, statement, reason, enhancedContext, contextCount, rawExpression, rawContextCount, enhancedContextList);
        }

        String statement;
        String reason = "[Incorrect]";
        Pair<Map<String, Integer>, String> enhancedContext;
        Map<String, Integer> contextCount;
        String rawExpression;
        Map<String, Integer> rawContextCount;
        ArrayList<String> enhancedContextList;
        public ContextAndStatement(String line) {
            this.context = new HashMap<>();
            this.contextCount = new HashMap<>();
            this.rawContextCount = new HashMap<>();
            int pos = 0;
            int num = 1;
            while (!line.startsWith("|-", pos)) {
                StringBuilder rawExpr = new StringBuilder();
                while (!line.startsWith(",", pos) && !line.startsWith("|-", pos)) {
                    rawExpr.append(line.charAt(pos));
                    pos += 1;
                }
                String expr = parseExpression(rawExpr.toString());
                context.put(expr, num);
                contextCount.put(expr, contextCount.getOrDefault(expr, 0) + 1);
                rawContextCount.put(rawExpr.toString(), rawContextCount.getOrDefault(rawExpr.toString(), 0) + 1);
                num += 1;
                if (line.startsWith(",", pos)) {
                    pos += 1;
                }
            }
            String _statement = parseExpression(line.substring(pos + 2));
            rawExpression = line.substring(pos + 2);
            Map<String, Integer> _enhancedContext = new HashMap<>(contextCount);
            enhancedContextList = new ArrayList<>();
            statement = _statement;
            while (true) {
                if (statement.length() < 3 || !statement.startsWith("->", 1)) {
                    enhancedContextList.add(normalizeExpr(statement));
                    break;
                }
                OperationAndOperands oao = new OperationAndOperands(statement);
                _enhancedContext.put(oao.operand1, _enhancedContext.getOrDefault(oao.operand1, 0) + 1);
                enhancedContextList.add(normalizeExpr(oao.operand1));
                statement = oao.operand2;
            }
            enhancedContext = new Pair<>(_enhancedContext, statement);
            statement = _statement;
        }
    }

    private static String normalizeExpr(String expr) {
        if (expr.charAt(0) == '(') {
            OperationAndOperands oao = new OperationAndOperands(expr);
            if (oao.numb == 1) {
                return "!" + normalizeExpr(oao.operand1);
            } else {
                return "(" + normalizeExpr(oao.operand1) + oao.operation + normalizeExpr(oao.operand2) + ")";
            }
        }
        return expr;
    }

    private static String parseExpression(String line) {
        return new ParserExpression(line).parseExpression();
    }

    public static class ParserExpression {
        String line;
        int pos;

        public ParserExpression(String line) {
            this.line = line + "#";
            pos = 0;
        }

        String parseExpression() {
            return e();
        }

        private String e() {
            String x = dij();
            if (skip("->")) {
                x = "(->," + x + "," + e() + ")";
            }
            return x;
        }

        private String dij() {
            StringBuilder x = new StringBuilder(con());
            while (skip("|")) {
                x = new StringBuilder("(|," + x + "," + con() + ")");
            }
            return x.toString();
        }

        private String con() {
            StringBuilder x = new StringBuilder(nt());
            while (skip("&")) {
                x = new StringBuilder("(&," + x + "," + nt() + ")");
            }
            return x.toString();
        }

        private String nt() {
            if (skip("(")) {
                String x = e();
                skip(")");
                return x;
            }
            if (skip("!")) {
                return "(!," + nt() + ")";
            }
            StringBuilder x = new StringBuilder();
            while (Character.isDigit(line.charAt(pos)) || Character.isLetter(line.charAt(pos)) || line.charAt(pos) == '\'') {
                x.append(line.charAt(pos));
                pos += 1;
            }
            return x.toString();
        }

        private boolean skip(String s) {
            if (line.startsWith(s, pos)) {
                pos += s.length();
                return true;
            }
            return false;
        }
    }

    public static class Pair<T, T1> {
        T first;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Pair<?, ?> pair = (Pair<?, ?>) o;
            return Objects.equals(first, pair.first) && Objects.equals(second, pair.second);
        }

        @Override
        public int hashCode() {
            return Objects.hash(first, second);
        }

        T1 second;
        public Pair(T first, T1 second) {
            this.first = first;
            this.second = second;
        }
    }

    public static class OperationAndOperands {
        String operation;
        String operand1;
        String operand2;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            OperationAndOperands that = (OperationAndOperands) o;
            return numb == that.numb && Objects.equals(operation, that.operation) && Objects.equals(operand1, that.operand1) && Objects.equals(operand2, that.operand2);
        }

        @Override
        public int hashCode() {
            return Objects.hash(operation, operand1, operand2, numb);
        }

        int numb;
        public OperationAndOperands(String statement) {
            operation = statement.substring(1, statement.indexOf(','));
            if (operation.equals("!")) {
                operand1 = statement.substring(statement.indexOf(',') + 1, statement.length() - 1);
                operand2 = "";
                numb = 1;
                return;
            }
            StringBuilder _operand1 = new StringBuilder();
            int countCommas = 0;
            int balance = 0;
            int ind = statement.indexOf(',') + 1;
            while (balance != 0 || countCommas == 0) {
                char sym = statement.charAt(ind);
                _operand1.append(sym);
                if (sym == '(') {
                    balance += 1;
                } else if (sym == ')') {
                    balance -= 1;
                } else if (sym == ',') {
                    countCommas += 1;
                }
                ind += 1;
            }
            if (statement.charAt(ind) != ',') {
                ind -= 1;
                _operand1.deleteCharAt(_operand1.length() - 1);
            }
            operand1 = _operand1.toString();
            operand2 = statement.substring(ind + 1, statement.length() - 1);
            numb = 2;
        }
    }
    public static class ProofItem {
        String rawExpression;
        String reason;

        public ProofItem(String rawExpression, String reason) {
            this.rawExpression = rawExpression;
            this.reason = reason;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ProofItem proofItem = (ProofItem) o;
            return Objects.equals(rawExpression, proofItem.rawExpression) && Objects.equals(reason, proofItem.reason);
        }

        @Override
        public int hashCode() {
            return Objects.hash(rawExpression, reason);
        }
    }
}
