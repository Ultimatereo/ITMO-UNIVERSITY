import sys


def parse_expression(_line):
    global line, pos
    line = _line + '#'
    pos = 0

    def skip(s):
        # print(pos)
        global line, pos
        if line.startswith(s, pos):
            pos += len(s)
            return True
        return False

    def e():
        x = dij()
        if skip('->'):
            x = '(->,' + x + ',' + e() + ')'
        return x

    def dij():
        x = con()
        while skip('|'):
            x = '(|,' + x + ',' + con() + ')'
        return x

    def con():
        x = nt()
        while skip('&'):
            x = '(&,' + x + ',' + nt() + ')'
        return x

    def nt():
        global pos, line
        if skip('('):
            x = e()
            skip(')')
            return x
        if skip('!'):
            return '(!,' + str(nt()) + ')'
        x = ''
        while line[pos].isdigit() or line[pos].isalpha() or line[pos] == "'":
            x += line[pos]
            pos += 1
        return x

    return e()


axioms = [
    'a->b->a',
    '(a->b)->(a->b->c)->(a->c)',
    'a->b->a&b',
    'a&b->a',
    'a&b->b',
    'a->a|b',
    'b->a|b',
    '(a->c)->(b->c)->(a|b->c)',
    '(a->b)->(a->!b)->(!a)',
    '!!a->a'
]

axioms = list(map(parse_expression, axioms))


def get_reasons(statements):
    for i in range(len(statements)):
        statement = statements[i]
        if check_for_axiom(statement):
            continue
        if check_for_hyp(statement):
            continue
        if check_for_mp(statements, i):
            continue
        if check_for_deduction(statements, i):
            continue


def get_operation_and_operands(statement):
    operation = statement[1:statement.find(',')]
    if operation == '!':
        return operation, statement[statement.find(',') + 1: len(statement) - 1], '', 1
    operand1 = ''
    count_commas = 0
    balance = 0
    ind = statement.find(',') + 1
    while balance != 0 or count_commas == 0:
        sym = statement[ind]
        operand1 += sym
        if sym == '(':
            balance += 1
        elif sym == ')':
            balance -= 1
        elif sym == ',':
            count_commas += 1
        ind += 1
    if statement[ind] != ',':
        ind -= 1
        operand1 = operand1[:len(operand1) - 1]
    operand2 = statement[ind + 1:len(statement) - 1]
    return operation, operand1, operand2, 2


def get_context_and_statement(line):
    context = {}
    context_count = {}
    pos = 0
    num = 1
    while not line.startswith("|-", pos):
        raw_expr = ''
        while not line.startswith(",", pos) and not line.startswith("|-", pos):
            raw_expr += line[pos]
            pos += 1
        expr = parse_expression(raw_expr)
        context[expr] = num
        if expr not in context_count:
            context_count[expr] = 0
        context_count[expr] = context_count[expr] + 1
        num += 1
        if line.startswith(",", pos):
            pos += 1
    _statement = parse_expression(line[pos + 2:])
    enhanced_context = context_count.copy()
    statement = _statement
    while True:
        if len(statement) < 3 or statement[1:3] != '->':
            break
        _, operand1, operand2, _ = get_operation_and_operands(statement)
        if operand1 not in enhanced_context:
            enhanced_context[operand1] = 0
        enhanced_context[operand1] = enhanced_context[operand1] + 1
        statement = operand2
    enhanced_context = [enhanced_context, statement]
    return {'context': context, 'statement': _statement,
            'reason': '[Incorrect]', 'enhanced_context': enhanced_context, 'context_count': context_count}


def check_for_hyp(line):
    statement = line['statement']
    context = line['context']
    if statement in context:
        line['reason'] = "[Hyp. " + str(context[statement]) + "]"
        return True
    return False


def equals_statement_to_axiom(statement, axiom, elements=None):
    if elements is None:
        elements = {}
    if axiom[0] != '(':
        if axiom in elements:
            if statement != elements[axiom]:
                return False
            return True
        elements[axiom] = statement
        return True
    if statement[0] == '(' and axiom[0] == '(':
        axOperation, axOperand1, axOperand2, axNum = get_operation_and_operands(axiom)
        axOperands = [axOperand1, axOperand2]
        operation, operand1, operand2, num = get_operation_and_operands(statement)
        operands = [operand1, operand2]
        if axOperation != operation:
            return False
        for i in range(axNum):
            axOperand = axOperands[i]
            operand = operands[i]
            b = equals_statement_to_axiom(operand, axOperand, elements)
            if not b:
                return False
        return True
    return False


def check_for_axiom(line):
    statement = line['statement']
    if statement[0] != '(':
        return False
    for i in range(len(axioms)):
        axiom = axioms[i]
        if equals_statement_to_axiom(statement, axiom):
            line['reason'] = '[Ax. sch. ' + str(i + 1) + "]"
            return True
    return False


def check_for_mp(statements, index):
    st = statements[index]['statement']
    context = statements[index]['context_count']
    statement_set = {}
    statement_second_set = {}
    for i in range(index):
        statement_for_check = statements[i]
        if statement_for_check['context_count'] == context:
            stC = statement_for_check['statement']
            if stC not in statement_set:
                statement_set[stC] = []
            statement_set[stC].append(i)
            if stC[0] != '(':
                continue
            operator, operand1, operand2, _ = get_operation_and_operands(stC)
            if operator == '->':
                if operand2 not in statement_second_set:
                    statement_second_set[operand2] = []
                statement_second_set[operand2].append((i, operand1))
    if st in statement_second_set:
        arr = statement_second_set[st]
        for i, operand in arr:
            if operand in statement_set:
                num = statement_set[operand]
                statements[index]['reason'] = '[M.P. ' + str(num[0] + 1) + ', ' + str(i + 1) + "]"
                return True
    return False


def check_for_deduction(statements, index):
    statement = statements[index]
    for i in range(index):
        prev_statement = statements[i]
        if statement['enhanced_context'] == prev_statement['enhanced_context']:
            statement['reason'] = '[Ded. ' + str(i + 1) + "]"
            return True
    return False


def main():
    sys.setrecursionlimit(2 ** 30)
    statements = []
    raw_statements = []
    # file = open('in', 'r')
    file = sys.stdin
    for _line in file:
        line = ''
        for x in _line:
            if not x.isspace():
                line += x
        statements.append(get_context_and_statement(line))
        raw_statements.append(line)
    file.close()
    get_reasons(statements)
    for i in range(len(statements)):
        print('[' + str(i + 1) + ']', end=' ')
        print(raw_statements[i], end=' ')
        print(statements[i]['reason'])
        # print(statements[i])


if __name__ == '__main__':
    main()
