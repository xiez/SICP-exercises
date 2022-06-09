# A Lisp evaluator implemented in Python

# utils --------------------
def is_pair(exp):
    if exp[0] == '(' and exp[-1] == ')':
        return True
    return False

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
    if exp[0] == '"' and exp[-1] == '"':
        return True
    return False

def trim_parenthese(string):
    return string.strip('()')

# eval & apply --------------------

def analyze(exp):
    exp = exp.strip()

    if not is_pair(exp):
        return True, exp

    if not validate_parenthese(exp):
        return False, None

    return True, exp

def eval_(exp, env):
    valid_exp, exp = analyze(exp)
    if not valid_exp:
        print(f"Invalid expression -- EVAL {exp}")
        return

    # primitive expressions
    if is_self_evaluating(exp):
        return self_evaluating(exp)
    elif is_variable(exp):
        return lookup_variable_value(exp, env)

    # special forms
    elif is_quote(exp):
        return get_quote_text(exp)

    # combinations
    elif is_combination(exp):
        return apply_(
            eval_(get_combination_operator(exp), env),
            list_of_values(get_combination_operands(exp), env)
        )

    # error
    else:
        print(f"Unknown expression type -- EVAL {exp}")
        return

def apply_(proc, args):
    if is_primitive_proc(proc):
        return run_python_apply(proc, args)
    elif is_compound_proc(proc):
        eval_sequence(
            get_procedure_body(proc),
            extend_environment(
                get_procedure_env(proc)
            )
        )
    else:
        print(f"Unknown procedure type -- APPLY {proc}")
        return

def is_self_evaluating(exp):
    if is_number(exp) or is_string(exp):
        return True
    return False

def self_evaluating(exp):
    if is_number(exp):
        try:
            return int(exp)
        except:
            return float(exp)
    else:
        return exp


def is_variable(exp):
    if is_pair(exp):
        return False

    if len(exp.split(' ')) > 1:
        return False
    return True

def lookup_variable_value(exp, env):
    return env.get(exp)

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
    return is_pair(exp)

def get_combination_operator(exp):
    return trim_parenthese(exp.split(' ')[0])

def get_combination_operands(exp):
    splits = exp.split(' ')     # TODO: handle nested expression
    rest = splits[1:]
    rest[-1] = trim_parenthese(rest[-1])
    return rest

def list_of_values(operands, env):
    ret = []
    for od in operands:
        ret.append(eval_(od, env))
    return ret

def is_primitive_proc(proc):
    return False if is_compound_proc(proc) else True

def is_compound_proc(proc):
    return False

def run_python_apply(proc, args):
    return proc(*args)

def eval_sequence(exps, env):
    ...

def get_procedure_body(proc):
    ...

def get_procedure_env(proc):
    ...

def extend_environment(env):
    ...

# TEST --------------------

ENV = {
    '+': lambda x, y: x + y,
    '-': lambda x, y: x - y,
    '*': lambda x, y: x * y,
    '/': lambda x, y: x / y,
    '=': lambda x, y: x == y,
    'display': lambda x: print(x),
    'a': 42,
}

assert eval_(
    '''
    1
    ''',
    ENV
) == 1


assert eval_(
    '''
    "1"
    ''',
    ENV
) == '"1"'

assert eval_(
    '''
    "hello world!"
    ''',
    ENV
) == '"hello world!"'

assert eval_(
    '''
    a
    ''',
    ENV
) == 42

assert eval_(
    '''
    (quote a)
    ''',
    ENV
) == 'a'

assert eval_(
    '''
    (+ 1 1)
    ''',
    ENV
) == 2

eval_(
    '''
    (display "hello-world!")
    ''',
    ENV
)
