import java.util.*;

public class Solution {
    public static void main(String[] args) {
        solve();
//        test(1000);
    }

//    private static void test(int num) {
//        for (int i = 0; i < num; i++) {
//            String _statement = RandomBooleanExpressionGenerator.generateRandomExpression();
//            System.out.println("STATEMENT: " + _statement);
//            solve(_statement, true);
//        }
//    }

    private static void solve() {
        Scanner sc = new Scanner(System.in);
        String _statement = sc.nextLine();
        sc.close();
        solve(_statement);
    }

    private static void solve(String statement) {
        solve(statement, true);
    }

    private static void solve(String _statement, boolean flag) {
        StringBuilder statement = new StringBuilder();
        for (int i = 0; i < _statement.length(); i++) {
            char c = _statement.charAt(i);
            if (!Character.isWhitespace(c)) {
                statement.append(c);
            }
        }
        Pair<NodeProof, String> proof = getProofComplete(statement.toString());
        if (flag) {
            if (proof.second != null) {
                System.out.println(proof.second);
            } else {
                proof.first.print();
//            proof.first.validate();
            }
        }
    }
    public static ExpressionNode beep = new ExpressionNode("_|_", false);
    private static Pair<NodeProof, String> getProofComplete(String statement) {
        ParserExpression parserExpression = new ParserExpression(statement);
        TreeSet<String> variables = parserExpression.variables;
        Map<Map<String, Boolean>, NodeProof> proofs = new HashMap<>();
        ExpressionNode expressionNode = new ExpressionNode(parserExpression.parseExpression());
        String message;
        for (int i = 0; i < Math.pow(2, variables.size()); i++) {
            boolean[] values = getValues(i, variables.size());
            Map<String, Boolean> valuesMap = new HashMap<>();
            int j = 0;
            for (String variable : variables) {
                valuesMap.put(variable, values[j]);
                j += 1;
            }
            boolean value = expressionNode.evaluate(valuesMap);
            if (!value) {
                message = "Formula is refutable [" + getValuesString(valuesMap) + "]";
                return new Pair<>(null, message);
            }
            NodeProof proof = createWithValueMap(expressionNode);
            proofs.put(valuesMap, proof);
//            proof.print();
//            System.out.println("------");
        }
        NodeProof proof = reduceProofs(
                proofs, variables);
        return new Pair<>(proof, null);
    }

    private static NodeProof reduceProofs(
            Map<Map<String, Boolean>, NodeProof> proofs,
            Set<String> variables) {
        Map<Map<String, Boolean>, NodeProof> oldProofs = proofs;
        Map<Map<String, Boolean>, NodeProof> newProofs;
        for (String variable : variables) {
            newProofs = new HashMap<>();
            for (Map<String, Boolean> values : oldProofs.keySet()) {
                NodeProof np1 = oldProofs.get(values);
                Map<String, Boolean> otherValues = new HashMap<>(values);
                otherValues.put(variable, !otherValues.get(variable));
                NodeProof np2 = oldProofs.get(otherValues);
                if (!values.get(variable)) {
                    NodeProof temp = np1;
                    np1 = np2;
                    np2 = temp;
                }
                NodeProof n = new NodeProof(
                        np1.expressionNode, "E|"
                );
                NodeProof eq = getEq(variable);
                n.children.add(np1);
                n.children.add(np2);
                n.children.add(eq);
                Map<String, Boolean> newValues = new HashMap<>(values);
                newValues.remove(variable);
                newProofs.put(newValues, n);
            }
            oldProofs = newProofs;
        }
        for (Map.Entry<Map<String, Boolean>, NodeProof> entry : oldProofs.entrySet()) {
            return entry.getValue();
        }
        throw new AssertionError("Unreachable");
    }

    private static NodeProof getEq(String variable) {
        ExpressionNode var = new ExpressionNode(variable, true);
        ExpressionNode notVar = new ExpressionNode("->", var, beep, false);
        ExpressionNode cur = new ExpressionNode("|", var, notVar, true);
        ExpressionNode notCur = new ExpressionNode("->", cur, beep, false);
        NodeProof n1 = new NodeProof(cur, "E!!");
        NodeProof n2 = new NodeProof(beep, "E->");
        NodeProof n3 = new NodeProof(notCur, "Ax");
        NodeProof n4 = new NodeProof(cur, "Ir|");
        NodeProof n5 = new NodeProof(notVar, "I->");
        NodeProof n6 = new NodeProof(beep, "E->");
        NodeProof n7 = new NodeProof(notCur, "Ax");
        NodeProof n8 = new NodeProof(cur, "Il|");
        NodeProof n9 = new NodeProof(var, "Ax");
        n1.children.add(n2);
        n2.children.add(n3);
        n2.children.add(n4);
        n4.children.add(n5);
        n5.children.add(n6);
        n6.children.add(n7);
        n6.children.add(n8);
        n8.children.add(n9);
        return n1;
    }

    private static NodeProof createWithValueMap(ExpressionNode expressionNode) {
        NodeProof npContext = new NodeProof(expressionNode);
        return npContext.createNodeForValuesMap();
    }

    private static String getValuesString(Map<String, Boolean> valuesMap) {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, Boolean> entry : valuesMap.entrySet()) {
            sb.append(entry.getKey());
            sb.append(":=");
            if (entry.getValue()) {
                sb.append("T");
            } else {
                sb.append("F");
            }
            sb.append(",");
        }
        if (sb.length() > 0) {
            sb.deleteCharAt(sb.length() - 1);
        }
        return sb.toString();
    }

    public static boolean[] getValues(int number, int size) {
        if (number == 0) {
            // Обработка случая, когда число равно 0
            return new boolean[size];
        }

        ArrayList<Boolean> binaryList = new ArrayList<>();
        while (number > 0) {
            binaryList.add(number % 2 == 1);
            number /= 2;
        }

        while (binaryList.size() < size) {
            binaryList.add(false);
        }

        // Преобразовываем ArrayList в массив булевых значений
        boolean[] binaryArray = new boolean[binaryList.size()];
        for (int i = 0; i < binaryList.size(); i++) {
            binaryArray[i] = binaryList.get(i);
        }

        return binaryArray;
    }

    public static class ParserExpression {
        private final String line;
        String expression = null;
        TreeSet<String> variables = new TreeSet<>();
        private int pos;

        public ParserExpression(String line) {
            this.line = line + "#";
            pos = 0;
            parseExpression();
        }

        String parseExpression() {
            if (expression == null) {
                expression = e();
            }
            return expression;
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
            if (skip("_|_")) {
                return "_|_";
            }
            StringBuilder x = new StringBuilder();
            while (Character.isDigit(line.charAt(pos)) || Character.isLetter(line.charAt(pos)) || line.charAt(pos) == '\'') {
                x.append(line.charAt(pos));
                pos += 1;
            }
            variables.add(x.toString());
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
        T1 second;

        public Pair(T first, T1 second) {
            this.first = first;
            this.second = second;
        }

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
    }

    public static class OperationAndOperands {
        String operation;
        String operand1;
        String operand2;
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
    }

    public static class ExpressionNode {
        private final String expression;
        public final String normalizedExpression;
        public ExpressionNode left = null;
        public ExpressionNode right = null;
        public Boolean value = null;
        public String operation = null;

        public ExpressionNode(String expression) {
            this.expression = expression;
            StringBuilder sb = new StringBuilder();
            normalizeExpr(expression, sb);
            this.normalizedExpression = sb.toString();
            constructTree();
        }

        public ExpressionNode(String variable, boolean value) {
            this.expression = variable;
            this.value = value;
            this.normalizedExpression = variable;
        }
        public ExpressionNode(String operation, ExpressionNode left, ExpressionNode right, Boolean value) {
            this.left = left;
            this.right = right;
            this.value = value;
            this.operation = operation;
            this.expression = "(" + operation + "," + left.expression + "," + right.expression + ")";
            this.normalizedExpression = "(" + left.normalizedExpression + operation + right.normalizedExpression + ")";
        }

        private void constructTree() {
            if (expression.charAt(0) != '(') {
                return;
            }
            OperationAndOperands oao = new OperationAndOperands(expression);
            operation = oao.operation;
            left = new ExpressionNode(oao.operand1);
            right = new ExpressionNode(oao.operand2);
        }

        public boolean evaluate(Map<String, Boolean> valuesMap) {
            if (operation == null) {
                if (expression.equals("_|_")) {
                    value = false;
                    return false;
                }
                value = valuesMap.get(expression);
                return value;
            }
            left.evaluate(valuesMap);
            right.evaluate(valuesMap);
            switch (operation) {
                case "|":
                    value = left.value || right.value;
                    break;
                case "&":
                    value = left.value && right.value;
                    break;
                case "->":
                    value = !left.value || right.value;
                    break;
            }
            return value;
        }
        private static void normalizeExpr(String expr, StringBuilder sb) {
            if (expr.charAt(0) == '(') {
                OperationAndOperands oao = new OperationAndOperands(expr);
                if (oao.numb == 1) {
                    sb.append("!");
                    normalizeExpr(oao.operand1, sb);
                } else {
                    sb.append("(");
                    normalizeExpr(oao.operand1, sb);
                    sb.append(oao.operation);
                    normalizeExpr(oao.operand2, sb);
                    sb.append(")");
                }
                return;
            }
            sb.append(expr);
        }
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            ExpressionNode that = (ExpressionNode) o;
            return Objects.equals(expression, that.expression) && Objects.equals(left, that.left) && Objects.equals(right, that.right) && Objects.equals(operation, that.operation);
        }

        @Override
        public int hashCode() {
            return Objects.hash(expression, left, right, operation);
        }
    }

    public static class NodeProof {
        final ExpressionNode expressionNode;
        String reason;
        ArrayList<NodeProof> children = new ArrayList<>();
        ArrayList<ExpressionNode> needToProof = new ArrayList<>();

        public NodeProof(ExpressionNode expressionNode) {
            this.expressionNode = expressionNode;
        }

        public NodeProof(ExpressionNode expressionNode, String reason) {
            this.expressionNode = expressionNode;
            this.reason = reason;
        }

        public NodeProof createNodeForValuesMap() {
            if (expressionNode.operation != null) {
                assert expressionNode.value != null;
                assert expressionNode.right.value != null;
                assert expressionNode.left.value != null;
                String operation = expressionNode.operation;
                boolean leftValue = expressionNode.left.value;
                boolean rightValue = expressionNode.right.value;
                ExpressionNode phi = expressionNode.left;
                ExpressionNode ksi = expressionNode.right;
                ExpressionNode cur = expressionNode;
                NodeProof N = createNodeSecondStep(phi, ksi, expressionNode, operation, leftValue, rightValue);
                if (N.needToProof.size() == 2) {
                    phi = N.needToProof.get(0);
                    ksi = N.needToProof.get(1);
                    NodeProof nP1Context = new NodeProof(phi);
                    NodeProof nP1 = nP1Context.createNodeForValuesMap();
                    NodeProof nP2Context = new NodeProof(ksi);
                    NodeProof nP2 = nP2Context.createNodeForValuesMap();
                    NodeProof n1 = new NodeProof(cur, "E|");
                    NodeProof n3 = new NodeProof(
                            new ExpressionNode("|", ksi, ksi, true), "Il|");
                    NodeProof n4 = new NodeProof(new ExpressionNode("|", phi, phi, true), "Il|");
                    NodeProof n2 = new NodeProof(cur, "E|");
                    n1.children.add(n2);
                    n1.children.add(n2);
                    n1.children.add(n3);
                    n2.children.add(N);
                    n2.children.add(N);
                    n2.children.add(n4);
                    n3.children.add(nP2);
                    n4.children.add(nP1);
                    return n1;
                } else if (N.needToProof.size() == 1) {
                    ExpressionNode letter = N.needToProof.get(0);
                    NodeProof npContext = new NodeProof(letter);
                    NodeProof np = npContext.createNodeForValuesMap();
                    NodeProof n1 = new NodeProof(cur, "E|");
                    NodeProof n3 = new NodeProof(
                            new ExpressionNode("|", letter, letter, true), "Il|");
                    n1.children.add(N);
                    n1.children.add(N);
                    n1.children.add(n3);
                    n3.children.add(np);
                    return n1;
                } else if (N.needToProof.size() == 0) {
                    return N;
                }
                throw new AssertionError("Unreachable amount of needed to proof");
            }
            return new NodeProof(expressionNode, "Ax");
        }

        private NodeProof createNodeSecondStep(
                ExpressionNode phi, ExpressionNode ksi,
                ExpressionNode cur, String operation,
                boolean value1, boolean value2) {
            if (operation == null ||
                    operation.equals("->") && ksi.expression.equals("_|_") &&
                            phi.operation == null && !phi.expression.equals("_|_")) {
                return new NodeProof(cur, "Ax");
            }

            switch (operation) {
                case "->": {
                    if (phi.expression.equals("_|_")) {
                        NodeProof n1 = new NodeProof(cur, "I->");
                        NodeProof n2 = new NodeProof(ksi, "E!!");
                        NodeProof n3 = new NodeProof(beep, "Ax");
                        n1.children.add(n2);
                        n2.children.add(n3);
                        return n1;
                    }

                    if (ksi.expression.equals("_|_")) {
                        if (phi.operation.equals("->") && phi.right.expression.equals("_|_")) {
                            ArrayList<ExpressionNode> needToProofHere = new ArrayList<>();
                            needToProofHere.add(phi.left);
                            NodeProof n1 = new NodeProof(cur, "I->");
                            n1.needToProof = needToProofHere;
                            NodeProof n2 = new NodeProof(beep, "E->");
                            NodeProof n3 = new NodeProof(phi, "Ax");
                            NodeProof n4 = new NodeProof(phi.left, "Ax");
                            n1.children.add(n2);
                            n2.children.add(n3);
                            n2.children.add(n4);
                            return n1;
                        }
                        ExpressionNode notCur = cur;
                        cur = phi;
                        phi = cur.left;
                        ksi = cur.right;
                        operation = cur.operation;
                        value1 = phi.value;
                        value2 = ksi.value;
                        if (operation.equals("->")) {
                            ExpressionNode notKsi = new ExpressionNode("->", ksi, beep, !phi.value);
                            NodeProof n1 = new NodeProof(notCur, "I->");
                            NodeProof n6 = new NodeProof(phi, "Ax");
                            NodeProof n5 = new NodeProof(cur, "Ax");
                            NodeProof n4 = new NodeProof(ksi, "E->");
                            n4.children.add(n5);
                            n4.children.add(n6);
                            NodeProof n3 = new NodeProof(notKsi, "Ax");
                            NodeProof n2 = new NodeProof(beep, "E->");
                            n2.children.add(n3);
                            n2.children.add(n4);
                            n1.children.add(n2);
                            n1.needToProof.add(phi);
                            n1.needToProof.add(notKsi);
                            return n1;
                        } else if (operation.equals("&")) {
                            if (!value1) {
                                ExpressionNode notPhi = new ExpressionNode("->", phi, beep, true);
                                NodeProof n1 = new NodeProof(notCur, "I->");
                                NodeProof n5 = new NodeProof(cur, "Ax");
                                NodeProof n4 = new NodeProof(phi, "El&");
                                n4.children.add(n5);
                                NodeProof n3 = new NodeProof(notPhi, "Ax");
                                NodeProof n2 = new NodeProof(beep, "E->");
                                n2.children.add(n3);
                                n2.children.add(n4);
                                n1.children.add(n2);
                                n1.needToProof.add(notPhi);
                                return n1;
                            } else if (!value2) {
                                ExpressionNode notKsi = new ExpressionNode("->", ksi, beep, true);
                                NodeProof n1 = new NodeProof(notCur, "I->");
                                NodeProof n5 = new NodeProof(cur, "Ax");
                                NodeProof n4 = new NodeProof(ksi, "Er&");
                                n4.children.add(n5);
                                NodeProof n3 = new NodeProof(notKsi, "Ax");
                                NodeProof n2 = new NodeProof(beep, "E->");
                                n2.children.add(n3);
                                n2.children.add(n4);
                                n1.children.add(n2);
                                n1.needToProof.add(notKsi);
                                return n1;
                            }
                        } else {
                            ExpressionNode notKsi = new ExpressionNode("->", ksi, beep, true);
                            ExpressionNode notPhi = new ExpressionNode("->", phi, beep, true);
                            NodeProof n1 = new NodeProof(notCur, "I->");
                            NodeProof n2 = new NodeProof(beep, "E|");
                            NodeProof n5 = new NodeProof(cur, "Ax");
                            NodeProof n3 = new NodeProof(beep, "E->");
                            NodeProof n6 = new NodeProof(notPhi, "Ax");
                            NodeProof n7 = new NodeProof(phi, "Ax");
                            NodeProof n4 = new NodeProof(beep, "E->");
                            NodeProof n8 = new NodeProof(notKsi, "Ax");
                            NodeProof n9 = new NodeProof(ksi, "Ax");
                            n1.children.add(n2);
                            n2.children.add(n3);
                            n2.children.add(n4);
                            n2.children.add(n5);
                            n3.children.add(n6);
                            n3.children.add(n7);
                            n4.children.add(n8);
                            n4.children.add(n9);
                            n1.needToProof.add(notPhi);
                            n1.needToProof.add(notKsi);
                            return n1;
                        }
                    }
                    if (value2) {
                        NodeProof n1 = new NodeProof(cur, "I->");
                        NodeProof n2Context = new NodeProof(ksi);
                        NodeProof n2 = n2Context.createNodeForValuesMap();
                        n1.children.add(n2);
                        return n1;
                    } else if (!value1) {
                        ExpressionNode notPhi = new ExpressionNode("->", phi, beep, !phi.value);
                        NodeProof n1 = new NodeProof(cur, "I->");
                        NodeProof n2 = new NodeProof(ksi, "E!!");
                        NodeProof n3 = new NodeProof(beep, "E->");
                        NodeProof n4 = new NodeProof(notPhi, "Ax");
                        NodeProof n5 = new NodeProof(phi, "Ax");
                        n1.children.add(n2);
                        n2.children.add(n3);
                        n3.children.add(n4);
                        n3.children.add(n5);
                        n1.needToProof.add(notPhi);
                        return n1;
                    }
                    throw new AssertionError("Unreachable case of ->");
                }
                case "&": {
                    if (value1 && value2) {
                        NodeProof n3Context = new NodeProof(ksi);
                        NodeProof n3 = n3Context.createNodeForValuesMap();

                        NodeProof n2Context = new NodeProof(phi);
                        NodeProof n2 = n2Context.createNodeForValuesMap();

                        NodeProof n1 = new NodeProof(cur, "I&");
                        n1.children.add(n2);
                        n1.children.add(n3);
                        return n1;
                    }
                    throw new AssertionError("Unreachable case of &");
                }
                case "|": {
                    if (value1) {
                        NodeProof n1 = new NodeProof(cur, "Il|");
                        NodeProof n2 = new NodeProof(phi, "Ax");
                        n1.children.add(n2);
                        n1.needToProof.add(phi);
                        return n1;
                    } else if (value2) {
                        NodeProof n1 = new NodeProof(cur, "Ir|");
                        NodeProof n2 = new NodeProof(ksi, "Ax");
                        n1.children.add(n2);
                        n1.needToProof.add(ksi);
                        return n1;
                    }
                    throw new AssertionError("Unreachable case of |");
                }
            }
            throw new AssertionError("Unreachable code in second step");
        }


        public void print() {
            print(0, new HashMap<>(), false);
        }

        private void print(int level, Map<String, Integer> context, boolean validateFlag) {
            if (children.size() > 3) {
                throw new AssertionError("SOMETHING IS VERY VERY WRONG");
            }
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            sb.append(level);
            sb.append("] ");
            for (Map.Entry<String, Integer> entry : context.entrySet()) {
                for (int i = 0; i < entry.getValue(); i++) {
                    sb.append(entry.getKey());
                    sb.append(",");
                }
            }
            if (context.size() > 0) {
                sb.deleteCharAt(sb.length() - 1);
            }
            sb.append("|-");
            sb.append(expressionNode.normalizedExpression);
            sb.append(" [");
            sb.append(reason);
            sb.append("]");

            //------------------------
//            if (validateFlag) {
//                String line = sb.toString();
//                String[] parts = line.split("\\s");
//                int tempLevel = Integer.parseInt(parts[0].substring(1, parts[0].length() - 1));
//                assert level == tempLevel;
//
//                String[] contextAndExpression = parts[1].split("\\|-");
//                String[] contextMapString = contextAndExpression[0].split(",");
//                Map<String, Integer> tempContext = new HashMap<>();
//                if (!contextMapString[0].equals("")) {
//                    for (String s : contextMapString) {
//                        String e = parseExpression(s);
//                        tempContext.put(e, tempContext.getOrDefault(e, 0) + 1);
//                    }
//                }
//                assert tempContext.equals(context);
//
//                String tempExpression = parseExpression(contextAndExpression[1]);
//                assert tempExpression.equals(expressionNode.expression);
//                assert parts[2].substring(1, parts[2].length() - 1).equals(reason);
//            }
            //------------------------
            String phi, ksi;

            Map<String, Integer> map1, map2, newContext, context1, context2;
//            Map<String, Integer> curContext = new HashMap<>(context);
//            ArrayList<Map<String, Integer>> childrenContext = new ArrayList<>();
            switch (reason) {
                case "Ax":
//                    if (validateFlag) validate(curContext, childrenContext);
                    break;
                case "E->":
                    context1 = new HashMap<>(context);
                    context2 = new HashMap<>(context);
//                    if (validateFlag) {
//                        childrenContext.add(context1);
//                        childrenContext.add(context2);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, context1, validateFlag);
                    children.get(1).print(level + 1, context2, validateFlag);
                    break;
                case "I&":
                    context1 = new HashMap<>(context);
                    context2 = new HashMap<>(context);
//                    if (validateFlag) {
//                        childrenContext.add(context1);
//                        childrenContext.add(context2);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, context1, validateFlag);
                    children.get(1).print(level + 1, context2, validateFlag);
                    break;
                case "I->":
                    phi = expressionNode.left.normalizedExpression;
                    context.put(phi, context.getOrDefault(phi, 0) + 1);
//                    if (validateFlag) {
//                        childrenContext.add(context);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, context, validateFlag);
                    break;
                case "E|":
                    phi = children.get(2).expressionNode.left.normalizedExpression;
                    ksi = children.get(2).expressionNode.right.normalizedExpression;
                    map1 = new HashMap<>(context);
                    map1.put(phi, map1.getOrDefault(phi, 0) + 1);
                    map2 = new HashMap<>(context);
                    map2.put(ksi, map2.getOrDefault(ksi, 0) + 1);
                    newContext = context;
//                    if (validateFlag) {
//                        childrenContext.add(map1);
//                        childrenContext.add(map2);
//                        childrenContext.add(newContext);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, map1, validateFlag);
                    children.get(1).print(level + 1, map2, validateFlag);
                    children.get(2).print(level + 1, newContext, validateFlag);
                    break;
                case "E!!":
                    phi = "(" + expressionNode.normalizedExpression + "->_|_)";
                    newContext = context;
                    newContext.put(phi, newContext.getOrDefault(phi, 0) + 1);
//                    if (validateFlag) {
//                        childrenContext.add(newContext);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, newContext, validateFlag);
                    break;
                default:
                    newContext = context;
//                    if (validateFlag) {
//                        childrenContext.add(newContext);
//                        validate(curContext, childrenContext);
//                    }
                    children.get(0).print(level + 1, newContext, validateFlag);
                    break;
            }
            System.out.println(sb);
        }

        public void validate(Map<String, Integer> context, ArrayList<Map<String, Integer>> childrenContext) {
            ExpressionNode en;
            Map<String, Integer> temp;
            switch (reason) {
                case "Ax":
                    assert children.size() == 0;
                    assert context.containsKey(expressionNode.expression);
                    break;
                case "E->":
                    assert children.size() == 2;
                    ExpressionNode left = children.get(0).expressionNode;
                    ExpressionNode right = children.get(1).expressionNode;
                    assert left.operation.equals("->") && left.right.expression.equals(expressionNode.expression);
                    assert right.expression.equals(left.left.expression);
                    assert childrenContext.get(0).equals(context);
                    assert childrenContext.get(1).equals(context);
                    break;
                case "I->":
                    assert children.size() == 1;
                    assert expressionNode.operation.equals("->");
                    ExpressionNode phi1 = expressionNode.left;
                    ExpressionNode ksi1 = expressionNode.right;
                    ExpressionNode ksi2 = children.get(0).expressionNode;
                    assert ksi1.equals(ksi2);
                    int number1 = childrenContext.get(0).getOrDefault(phi1.expression, 0);
                    int number2 = context.getOrDefault(phi1.expression, 0);
                    assert number1 == number2 + 1;
                    temp = new HashMap<>(context);
                    temp.put(phi1.expression, temp.getOrDefault(phi1.expression, 0) + 1);
                    assert temp.equals(childrenContext.get(0));
                    break;
                case "I&":
                    assert children.size() == 2;
                    assert childrenContext.get(0).equals(context);
                    assert childrenContext.get(1).equals(context);
                    assert expressionNode.operation.equals("&");
                    assert expressionNode.left.expression.equals(children.get(0).expressionNode.expression);
                    assert expressionNode.right.expression.equals(children.get(1).expressionNode.expression);
                    break;
                case "El&":
                    assert children.size() == 1;
                    assert childrenContext.get(0).equals(context);
                    assert children.get(0).expressionNode.operation.equals("&");
                    en = children.get(0).expressionNode;
                    assert en.left.expression.equals(expressionNode.expression);
                    break;
                case "Er&":
                    assert children.size() == 1;
                    assert childrenContext.get(0).equals(context);
                    assert children.get(0).expressionNode.operation.equals("&");
                    en = children.get(0).expressionNode;
                    assert en.right.expression.equals(expressionNode.expression);
                    break;
                case "Il|":
                    assert children.size() == 1;
                    assert childrenContext.get(0).equals(context);
                    assert expressionNode.operation.equals("|");
                    en = children.get(0).expressionNode;
                    assert en.expression.equals(expressionNode.left.expression);
                    break;
                case "Ir|":
                    assert children.size() == 1;
                    assert childrenContext.get(0).equals(context);
                    assert expressionNode.operation.equals("|");
                    en = children.get(0).expressionNode;
                    assert en.expression.equals(expressionNode.right.expression);
                    break;
                case "E|":
                    assert children.size() == 3;
                    assert context.equals(childrenContext.get(2));
                    assert children.get(2).expressionNode.operation.equals("|");
                    ExpressionNode phi = children.get(2).expressionNode.left;
                    ExpressionNode ksi = children.get(2).expressionNode.right;
                    temp = new HashMap<>(context);
                    Map<String, Integer> temp1 = new HashMap<>(temp);
                    temp.put(phi.expression, temp.getOrDefault(phi.expression, 0) + 1);
                    assert temp.equals(childrenContext.get(0));
                    temp1.put(ksi.expression, temp1.getOrDefault(ksi.expression, 0) + 1);
                    assert temp1.equals(childrenContext.get(1));
                    assert children.get(0).expressionNode.equals(expressionNode);
                    assert children.get(1).expressionNode.equals(expressionNode);
                    break;
                case "E!!":
                    assert children.size() == 1;
                    temp = new HashMap<>(context);
                    phi = expressionNode;
                    String notPhi = "(->," + phi.expression + ",_|_)";
                    temp.put(notPhi, temp.getOrDefault(notPhi, 0) + 1);
                    assert temp.equals(childrenContext.get(0));
                    assert children.get(0).expressionNode.expression.equals("_|_");
                    break;
                default:
                    throw new AssertionError("Unknown type of link " + reason);
            }
//            for (NodeProof child : children) {
//                child.validate(context, childrenContext);
//            }
        }
    }
}
