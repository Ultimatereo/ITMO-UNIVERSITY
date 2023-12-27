import copy
import sys
from typing import List, Dict, Tuple, Union

known_recipes: List[List[Dict[str, str]]] = []


def parse_expression(_line: str) -> str:
    global line, pos
    line = _line + '#'
    pos = 0

    def skip(s: str) -> bool:
        global line, pos
        if line.startswith(s, pos):
            pos += len(s)
            return True
        return False

    def e() -> str:
        x = dij()
        if skip('->'):
            x = '(->,' + x + ',' + e() + ')'
        return x

    def dij() -> str:
        x = con()
        while skip('|'):
            x = '(|,' + x + ',' + con() + ')'
        return x

    def con() -> str:
        x = nt()
        while skip('&'):
            x = '(&,' + x + ',' + nt() + ')'
        return x

    def nt() -> str:
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


def get_reasons(statements: List[Dict[str, Union[str, Dict[str, int]]]]) -> None:
    statement_sets = {}
    statement_second_sets = {}
    map_for_deduction = {}
    for i in range(len(statements)):
        statement = statements[i]
        if check_for_axiom(statement):
            pass
        elif check_for_hyp(statement):
            pass
        elif check_for_mp(statements, i, statement_sets, statement_second_sets):
            pass
        elif check_for_deduction(statements, i, map_for_deduction):
            pass
        if statement['enhanced_context'] not in map_for_deduction:
            map_for_deduction[statement['enhanced_context']] = i
        if statement['context_count'] not in statement_sets:
            statement_sets[statement['context_count']] = {}
            statement_second_sets[statement['context_count']] = {}
        statement_set = statement_sets[statement['context_count']]
        statement_second_set = statement_second_sets[statement['context_count']]
        stC = statement['statement']
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


def get_operation_and_operands(statement: str) -> Tuple[str, str, str, int]:
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


def get_context_and_statement(line: str) -> Dict[str, Union[Dict[str, int], str, List[str]]]:
    context = {}
    context_count = {}
    raw_context_count = {}
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
            raw_context_count[raw_expr] = 0
        context_count[expr] = context_count[expr] + 1
        raw_context_count[raw_expr] = raw_context_count[raw_expr] + 1
        num += 1
        if line.startswith(",", pos):
            pos += 1
    _statement = parse_expression(line[pos + 2:])
    raw_expression = line[pos + 2:]
    enhanced_context = context_count.copy()
    enhanced_context_list = []
    statement = _statement
    while True:
        if len(statement) < 3 or statement[1:3] != '->':
            enhanced_context_list.append(normalize_expr(statement))
            break
        _, operand1, operand2, _ = get_operation_and_operands(statement)
        if operand1 not in enhanced_context:
            enhanced_context[operand1] = 0
        enhanced_context[operand1] = enhanced_context[operand1] + 1
        enhanced_context_list.append(normalize_expr(operand1))
        statement = operand2
    enhanced_context = (tuple(sorted(enhanced_context.items())), statement)
    return {'context': context, 'statement': _statement,
            'reason': '[Incorrect]',
            'enhanced_context': enhanced_context,
            'context_count': tuple(sorted(context_count.items())),
            'raw_expression': raw_expression, 'raw_context_count': raw_context_count,
            'enhanced_context_list': enhanced_context_list}


# Остальной код остается без изменений

def check_for_hyp(line: Dict[str, Union[str, Dict[str, int]]]) -> bool:
    statement = line['statement']
    context = line['context']
    if statement in context:
        line['reason'] = "[Hyp. " + str(context[statement]) + "]"
        return True
    return False


def equals_statement_to_axiom(statement: str, axiom: str, elements: Dict[str, str] = None) -> bool:
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


def check_for_axiom(line: Dict[str, Union[str, Dict[str, int]]]) -> bool:
    statement = line['statement']
    if statement[0] != '(':
        return False
    for i in range(len(axioms)):
        axiom = axioms[i]
        if equals_statement_to_axiom(statement, axiom):
            line['reason'] = '[Ax. sch. ' + str(i + 1) + "]"
            return True
    return False

def check_for_mp(statements: List[Dict[str, Union[str, Dict[str, int]]]], index: int,
                 statement_sets: Dict[str | Dict[str, int], Dict],
                 statement_second_sets: Dict[str, Dict]) -> bool:
    st = statements[index]['statement']
    context = statements[index]['context_count']
    if context in statement_sets:
        statement_set = statement_sets[context]
        statement_second_set = statement_second_sets[context]
        if st in statement_second_set:
            arr = statement_second_set[st]
            for i, operand in arr:
                if operand in statement_set:
                    num = statement_set[operand]
                    statements[index]['reason'] = '[M.P. ' + str(num[0] + 1) + ', ' + str(i + 1) + "]"
                    return True
    return False


def check_for_deduction(statements: List[Dict[str, Union[str, Dict[str, int]]]], index: int,
                        map_for_deduction: Dict[str, int]) -> bool:
    statement = statements[index]
    if statement['enhanced_context'] in map_for_deduction:
        statement['reason'] = '[Ded. ' + str(map_for_deduction[statement['enhanced_context']] + 1) + "]"
        return True
    return False


def delete_hyp(hyp: str, cur_proof: List[Dict[str, str]]) -> List[Dict[str, str]]:
    new_proof = []
    alpha = hyp
    old_to_new = [-1 for _ in range(len(cur_proof))]
    for i in range(len(cur_proof)):
        st = cur_proof[i]
        dn = st['raw_expression']
        n = len(new_proof)
        if dn == alpha:
            new_proof.append(
                {'raw_expression': alpha + '->(' + alpha + '->' + alpha + ')',
                 'reason': '[Ax. sch. 1]'})
            new_proof.append(
                {'raw_expression':
                     '(' + alpha + '->(' + alpha + '->' + alpha + '))->' +
                     '(' + alpha + '->(' + alpha + '->' + alpha + ')->' + alpha + ')->' +
                     '(' + alpha + '->' + alpha + ')',
                 'reason': '[Ax. sch. 2]'})
            new_proof.append(
                {'raw_expression':
                     '(' + alpha + '->(' + alpha + '->' + alpha + ')->' + alpha + ')->' +
                     '(' + alpha + '->' + alpha + ')',
                 'reason': '[M.P. ' + str(n + 1) + ', ' + str(n + 2) + ']'})
            new_proof.append(
                {'raw_expression':
                     alpha + '->(' + alpha + '->' + alpha + ')->' + alpha,
                 'reason': '[Ax. sch. 1]'})
            new_proof.append(
                {'raw_expression':
                     alpha + '->' + alpha,
                 'reason': '[M.P. ' + str(n + 4) + ', ' + str(n + 3) + ']'
                 })
            old_to_new[i] = n + 5
        elif st['reason'][1] == 'M':
            reason = st['reason']
            j = int(reason[reason.find(' ') + 1:reason.find(',')]) - 1
            k = int(reason[reason.find(',') + 2:len(reason) - 1]) - 1
            dj = cur_proof[j]['raw_expression']
            new_proof.append(
                {'raw_expression':
                     '(' + alpha + '->' + dj + ')->' +
                     '(' + alpha + '->' + dj + '->' + dn + ')->' +
                     '(' + alpha + '->' + dn + ')',
                 'reason': '[Ax. sch. 2]'
                 }
            )
            new_proof.append(
                {'raw_expression':
                     '(' + alpha + '->' + dj + '->' + dn + ')->' +
                     '(' + alpha + '->' + dn + ')',
                 'reason': '[M.P. ' + str(old_to_new[j]) + ', ' + str(n + 1) + ']'
                 }
            )
            new_proof.append(
                {'raw_expression':
                     alpha + '->' + dn,
                 'reason': '[M.P. ' + str(old_to_new[k]) + ', ' + str(n + 2) + ']'
                 }
            )
            old_to_new[i] = n + 3
        else:
            new_proof.append(
                {'raw_expression':
                     dn + '->' + alpha + '->' + dn,
                 'reason': '[Ax. sch. 1]'
                 }
            )
            new_proof.append({'raw_expression': dn, 'reason': '[Ax. or Hyp.]'})
            new_proof.append({
                'raw_expression': alpha + '->' + dn,
                'reason': '[M.P. ' + str(n + 2) + ', ' + str(n + 1) + ']'
            })
            old_to_new[i] = n + 3
    for i in range(len(new_proof)):
        new_proof[i]['raw_expression'] = normalize(new_proof[i]['raw_expression'])
    return new_proof


def add_hyp(hyp: str, rest: str, cur_proof: List[Dict[str, str]]) -> None:
    n = len(cur_proof)
    cur_proof.append(
        {'raw_expression': hyp,
         'reason': '[Hyp]'})
    cur_proof.append(
        {'raw_expression': rest,
         'reason': '[M.P. ' + str(n + 1) + ', ' + str(n) + ']'})


def normalize(line: str) -> str:
    return normalize_expr(parse_expression(line))


def normalize_expr(expr: str) -> str:
    _expr = expr
    if _expr[0] == '(':
        operation, operand1, operand2, numb = get_operation_and_operands(_expr)
        if numb == 1:
            return '!' + normalize_expr(operand1)
        else:
            return '(' + normalize_expr(operand1) + operation + normalize_expr(operand2) + ')'
    return _expr


def construct_deduction_another(index_new: int, index_old: int,
                                statements: List[Dict[str, Union[str, Dict[str, int]]]]) -> List[Dict[str, str]]:
    statement = statements[index_new]
    prev_statement = statements[index_old]
    statement_new = statement['enhanced_context_list']
    statement_old = prev_statement['enhanced_context_list']
    cur_proof = get_proof_another(statements, index_old)
    i = len(statement_new) - 2
    j = len(statement_old) - 2
    while i >= 0 and j >= 0:
        if statement_new[i] == statement_old[j]:
            i -= 1
            j -= 1
        else:
            break
    for jj in range(j + 1):
        hyp = statement_old[jj]
        rest = '->'.join(statement_old[jj + 1:])
        add_hyp(hyp, rest, cur_proof)
    for ii in range(i, -1, -1):
        hyp = statement_new[ii]
        cur_proof = delete_hyp(hyp, cur_proof)
    return cur_proof


def get_proof_another(statements: List[Dict[str, Union[str, Dict[str, int]]]], index: int) -> List[Dict[str, str]]:
    global known_recipes
    if len(known_recipes[index]) != 0:
        return copy.deepcopy(known_recipes[index])
    statement = statements[index]
    if (reason := statement['reason'])[1] == 'D':
        start = reason.find('.')
        end = len(reason) - 1
        number = int(reason[start + 2:end]) - 1
        known_recipes[index] = construct_deduction_another(index, number, statements)
    elif reason[1] == 'M':
        numbers = []
        i = 0
        while i < len(reason):
            if not reason[i].isnumeric():
                i += 1
            else:
                num = ''
                while reason[i].isnumeric():
                    num += reason[i]
                    i += 1
                numbers.append(int(num) - 1)
        proof1 = get_proof_another(statements, numbers[0])
        proof2 = get_proof_another(statements, numbers[1])
        for j in range(len(proof2)):
            reason = proof2[j]['reason']
            if reason[1] == 'M':
                numbers1 = []
                i = 0
                while i < len(reason):
                    if not reason[i].isnumeric():
                        i += 1
                    else:
                        num = ''
                        while reason[i].isnumeric():
                            num += reason[i]
                            i += 1
                        numbers1.append(int(num))
                proof2[j]['reason'] = '[M.P. ' + str(numbers1[0] + len(proof1)) + ', ' + \
                                      str(numbers1[1] + len(proof1)) + ']'
        proof = []
        for p in proof1:
            proof.append(p)
        for p in proof2:
            proof.append(p)
        proof.append({
            'raw_expression': normalize(statement['raw_expression']),
            'reason': '[M.P. ' + str(len(proof1)) + ', ' + str(len(proof)) + ']'
        })
        known_recipes[index] = proof
    else:
        known_recipes[index] = [{
            'raw_expression': normalize(statement['raw_expression']),
            'reason': '[Hyp. or Ax.]'
        }]
    return copy.deepcopy(known_recipes[index])


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

    # for i in range(len(statements)):
    #     print('[' + str(i + 1) + ']', end=' ')
    #     print(raw_statements[i], end=' ')
    #     print(statements[i]['reason'])
    # for i in range(len(statements)):
    #     print(statements[i])
    global known_recipes
    known_recipes = [[] for _ in range(len(statements))]
    proof = get_proof_another(statements, len(statements) - 1)
    print(raw_statements[-1])

    # _proof = [get_context_and_statement('|-' + el['raw_expression']) for el in proof]
    # get_reasons(_proof)
    # print('\n'.join(map(str, [el['reason'] for el in _proof])))

    for i in range(len(proof)):
        print(proof[i]['raw_expression'])
        # print(_proof[i]['reason'], proof[i]['reason'])


if __name__ == '__main__':
    main()
