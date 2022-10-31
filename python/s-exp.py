# lisp expression LL(1) grammar:
# expr -> "(" operator operands ")"
# operator -> "string"|expr
# operands -> operand*
# operand -> "int"|"float"|"string"|expr

#################### lexical analyzer
def tokenize(lines):
    print(f'lexical analyze lines: {lines}')

    def _tokenize(line):
        splits = line.replace('(', '( ').replace(')', ' )').split(' ')
        return [s for s in splits if s]

    ret = []
    for line in lines.split('\n'):
        if not line.strip():
            continue
        ret += _tokenize(line.strip())
    return ret

# line = '(+ (* 1 3) (/ 4 2))'
# assert tokenize(line) == ['(', '+', '(', '*', '1', '3', ')', '(', '/', '4', '2', ')', ')']

#################### syntactic analyzer
# recursive descent parsing
def analyze(tokens):
    print(f'syntactic analyze (parse) tokens: {tokens}')

    def expr():
        assert tokens.pop(0) == '('           # remove (
        op = operator()
        args = operands()
        assert tokens.pop(0) == ')'           # remove )
        return [op] + args

    def operator():
        try:
            if tokens[0] == '(':
                return expr()
            else:
                return tokens.pop(0)
        except IndexError:
            raise SyntaxError("expected a ')' to close '('")

    def operands():
        if not tokens:
            return []
        ret = []
        try:
            while tokens[0] != ')':
                ret.append(operand())
            return ret
        except IndexError:
            raise SyntaxError("expected a ')' to close '('")

    def operand():
        if tokens[0] == '(':
            return expr()
        else:
            return analyze_operand(tokens.pop(0))

    ret = expr()
    if tokens:
        raise SyntaxError(f"unexpected tokens: {tokens}")
    return ret

def analyze_operand(token):
    try:
        return int(token)
    except (TypeError, ValueError):
        try:
            return float(token)
        except (TypeError, ValueError):
            return token

#################### parser
def parse(line):
    return analyze(tokenize(line))

#################### test normal cases
lines = '(newline)'
assert parse(lines) == ['newline']

lines = '(f 1)'
assert parse(lines) == ['f', 1]

lines = '(f (g 1) 2)'
assert parse(lines) == ['f', ['g', 1], 2]

lines = '''
(define (accumulate op init seq)
  (if (null? seq) init
      (op
       (car seq)
       (accumulate op init (cdr seq)))))
'''
assert parse(lines) == ['define', ['accumulate', 'op', 'init', 'seq'], ['if', ['null?', 'seq'], 'init', ['op', ['car', 'seq'], ['accumulate', 'op', 'init', ['cdr', 'seq']]]]]

lines = '''
(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 2)) (fib (- n 1)))))
'''
assert parse(lines) == ['define', ['fib', 'n'], ['if', ['<', 'n', 2], 'n', ['+', ['fib', ['-', 'n', 2]], ['fib', ['-', 'n', 1]]]]]

lines = '''((lambda (x) x) 1) '''
assert parse(lines) == [['lambda', ['x'], 'x'], 1]

lines = '''
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
'''
assert parse(lines) == ['define', ['fixed-point', 'f', 'first-guess'], ['define', ['close-enough?', 'v1', 'v2'], ['<', ['abs', ['-', 'v1', 'v2']], 'tolerance']], ['define', ['try', 'guess'], ['let', [['next', ['f', 'guess']]], ['if', ['close-enough?', 'guess', 'next'], 'next', ['try', 'next']]]], ['try', 'first-guess']]


# test error cases
try:
    parse('((lambda (x) x) 1)))')
    assert False
except SyntaxError:
    assert True

try:
    parse('((lambda (x) x) 1')
    assert False
except SyntaxError:
    assert True

try:
    parse('((')
    assert False
except SyntaxError:
    assert True

print('Done.')
