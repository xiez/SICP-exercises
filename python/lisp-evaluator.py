# A Lisp evaluator implemented in Python
from pprint import pprint


# utils --------------------
def is_pair(exp):
    return True if isinstance(exp, list) else False

def validate_parenthese(exp):
    # TODO
    return True

def is_number(exp):
    try:
        if isinstance(int(exp), int) or isinstance(float(exp), float):
            return True
    except:
        return False

def is_string(exp):
    return True if exp[0] == '"' else False


# tokenize & parse --------------------
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

def analyze(tokens):
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

def parse(line):
    try:
        return analyze(tokenize(line))
    except:
        return tokenize(line)[0]

# eval & apply --------------------
def eval_(exp, env):
    print(f'eval_ {exp}')
    if is_self_evaluating(exp):  # 42, 3.14, "hello"
        return self_evaluating(exp)
    elif is_variable(exp):      # foo + - * /
        return lookup_variable_value(exp, env)
    # elif is_quote(exp):
    #     return get_quote_text(exp)
    elif is_lambda(exp):
        return make_compound_procedure(
            lambda_parameters(exp),
            lambda_body(exp),
            env,
        )
    elif is_definination(exp):
        # print(f'is define {exp}')
        return eval_defination(exp, env)
    elif is_combination(exp):   # ['+', 1, 1]
        return apply_(
            eval_(get_combination_operator(exp), env),
            list_of_values(get_combination_operands(exp), env)
        )
    else:
        raise Exception(f"Unknown expression type -- EVAL {exp}")

def apply_(proc, args):
    if is_primitive_proc(proc):
        return run_python_apply(proc, args)
    elif is_compound_procedure(proc):
        return eval_sequence(
            procedure_body(proc),
            extend_environment(
                procedure_params(proc),
                args,
                procedure_environment(proc)
            )
        )
    else:
        raise Exception(f"Unknown procedure type -- APPLY {proc}")

def is_self_evaluating(exp):
    return True if is_number(exp) or is_string(exp) else False

def self_evaluating(exp):
    try:
        return int(exp)
    except ValueError:
        try:
            return float(exp)
        except ValueError:
            return exp

def is_variable(exp):
    return not is_pair(exp)

def set_variable(var, value, env):
    env[0][var] = value

def lookup_variable_value(var, env):
    for a_frame in env:
        val = a_frame.get(var)
        if val:
            return val
    raise Exception(f"{var} is not defined in the env")

def is_lambda(exp):
    return exp[0] == 'lambda'

def make_lambda(params, body):
    # params: ["arg1", ...] or []
    # body: ["a", ["+", 1, 2], ...]
    if not body:
        raise Exception("body is empty -- make_lambda")
    lambda_exp = ['lambda']
    lambda_exp.append(params)
    lambda_exp.append(body)
    return lambda_exp

def lambda_parameters(exp):
    return exp[1]

def lambda_body(exp):
    return exp[2]

def is_compound_procedure(exp):
    return exp[0] == 'procedure'

def make_compound_procedure(params, body, env):
    proc_exp = ['procedure']
    proc_exp.append(params)
    proc_exp.append(body)
    proc_exp.append(env)
    return proc_exp

def procedure_params(exp):
    return exp[1]

def procedure_body(exp):
    return exp[2]

def procedure_environment(exp):
    return exp[3]

def is_definination(exp):
    return exp[0] == 'define'

def eval_defination(exp, env):
    # (define a 42)
    # (define (f arg1 arg2) body)
    def defination_variable():
        if is_pair(exp[1]):
            return exp[1][0]
        else:
            return exp[1]

    def defination_value():
        if is_pair(exp[1]):
            params = exp[1][1:]
            body = exp[2:]
            return make_lambda(params, body)
        else:
            return exp[2]

    set_variable(
        defination_variable(),
        eval_(defination_value(), env),
        env
    )

def is_quote(exp):
    if not is_pair(exp):
        return False

    splits = exp.split(' ')
    if len(splits) > 1 and splits[0] == '(quote':
        return True
    return False

def get_quote_text(exp):
    assert is_quote(exp), exp
    splits = exp.split(' ')
    return trim_parenthese(''.join(splits[1:]))

def is_combination(exp):
    # print(f'is_combination: {exp}')
    return is_pair(exp)

def get_combination_operator(exp):
    return exp[0]

def get_combination_operands(exp):
    return exp[1:]

def list_of_values(operands, env):
    return [eval_(o, env) for o in operands]

def is_primitive_proc(proc):
    return callable(proc)

def run_python_apply(proc, args):
    return proc(*args)

def eval_sequence(exps, env):
    # use last value of exps as the return value
    ret = None
    for exp in exps:
        ret = eval_(exp, env)
    return ret

def extend_environment(params, vals, env=[]):
    assert len(params) == len(vals)
    assert isinstance(env, list)

    new_env = {}
    for idx, param in enumerate(params):
        new_env[param] = vals[idx]
    return [new_env] + env

# TEST --------------------

ENV = extend_environment(
    ['+', '-', '*', '/', '=', 'display'],
    [lambda x, y: x + y,
     lambda x, y: x - y,
     lambda x, y: x * y,
     lambda x, y: x / y,
     lambda x, y: x == y,
     print,
    ], []
)

# assert eval_(
#     '''
#     1
#     ''',
#     ENV
# ) == 1


# assert eval_(
#     '''
#     "1"
#     ''',
#     ENV
# ) == '"1"'

# assert eval_(
#     '''
#     "hello world!"
#     ''',
#     ENV
# ) == '"hello world!"'

# assert eval_(
#     '''
#     a
#     ''',
#     ENV
# ) == 42

# assert eval_(
#     '''
#     (quote a)
#     ''',
#     ENV
# ) == 'a'

print(eval_(parse(
    '''
    (+ 1 1)
    '''
), ENV))

print(eval_(parse(
    '''
    (* (+ 1 1) (- 3 1))
    '''
), ENV))

# print(eval_(parse(
#     '''
#     (+ (+ 1 1) 1 2)
#     '''
# ), ENV))

eval_(parse(
    '''
    (display "hello-world!")
    '''
), ENV)

eval_(parse('''
(define a 42)
'''), ENV)

print(eval_(parse('''
(+ a 1)
'''), ENV))

eval_(parse('''
(define a 10)
'''), ENV)

print(eval_(parse('''
(+ a 1)
'''), ENV))

eval_(parse('''
(define (f b)
(+ b 1)
)
'''), ENV)

# print(eval_(parse('''
# f
# '''), ENV))

print('(f 10) -> ', eval_(parse('''
(f 10)
'''), ENV))


eval_(parse('''
(define (g b)
(define (h)
 (+ 1 b))
(h)
)
'''), ENV)

print('(g 20) -> ', eval_(parse('''
(g 20)
'''), ENV))
