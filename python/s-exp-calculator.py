# S-exp := (operator operand1 ... operandN)
# operand := int|float|<S-exp>
# E.g. (+ 1 (* 1 3) (/ 4 2))

# lexical analyzer
def tokenize(lines):
    def _tokenize(line):
        splits = line.replace('(', '( ').replace(')', ' )').split(' ')
        return [s for s in splits if s]

    ret = []
    for line in lines.split('\n'):
        if not line.strip():
            continue
        ret += _tokenize(line.strip())
    return ret

line = '(+ (* 1 3) (/ 4 2))'
assert tokenize(line) == ['(', '+', '(', '*', '1', '3', ')', '(', '/', '4', '2', ')', ')']

# lines = '''((lambda (x) x) 1) '''
# print(tokenize(lines))
print('lexical analyzer ok ...')

# syntactic analyzer
def analyze(tokens):
    token = analyze_token(tokens.pop(0))
    if type(token) in (int, float):
        return token

    assert token == '('

    operator = analyze_token(tokens.pop(0))
    return [
        operator,
        analyze_operands(tokens),
    ]

def analyze_token(token):
    try:
        return int(token)
    except (TypeError, ValueError):
        try:
            return float(token)
        except (TypeError, ValueError):
            return token

def analyze_operands(tokens):
    operands = []
    while True:
        token = tokens[0]
        if token == ')':
            break
        operands.append(analyze(tokens))

    tokens.pop(0)               # remove )
    return operands

# parser
def parse(line):
    return analyze(tokenize(line))

# test
assert parse('3.14') == 3.14
assert parse('(+ (* 1 3) (/ 4 2))') == ['+', [['*', [1, 3]], ['/', [4, 2]]]]
assert parse("(+ (- 9 (abs -1)) (* 3 4) (/ 5 2))") == ['+', [['-', [9, ['abs', [-1]]]], ['*', [3, 4]], ['/', [5, 2]]]]

lines = '''
(add (mul 2 1)
     (div 4 (abs (sub 1 2))))
'''
assert parse(lines) == ['add', [['mul', [2, 1]], ['div', [4, ['abs', [['sub', [1, 2]]]]]]]]

print('parser test ok')
