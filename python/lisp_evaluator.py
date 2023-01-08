# A Lisp evaluator implemented in Python
import operator as op
from pprint import pprint


# utils --------------------
def is_pair(exp):
    return True if isinstance(exp, list) else False

def validate_parenthese(exp):
    # TODO
    return True

def is_number_string(exp):
    try:
        int(exp)
        return True
    except:
        try:
            float(exp)
            return True
        except:
            return False

def is_quote_string(exp):
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
    # print(f'eval_ {exp}, {env}')
    if is_self_evaluating(exp):  # 42, 3.14, "hello"
        return self_evaluating(exp)
    elif is_variable(exp):      # foo + - * / null
        return lookup_variable_value(exp, env)
    # elif is_quote(exp):
    #     return get_quote_text(exp)
    elif is_if(exp):
        return eval_if(exp, env)
    elif is_lambda(exp):
        return make_compound_procedure(
            lambda_parameters(exp),
            lambda_body(exp),
            env,
        )
    elif is_definination(exp):
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
    if isinstance(exp, int) or isinstance(exp, float):
        return True

    return True if is_number_string(exp) or \
        is_quote_string(exp) else False

def self_evaluating(exp):
    if isinstance(exp, int) or isinstance(exp, float):
        return exp
    if is_number_string(exp):
        try:
            return int(exp)
        except:
            return float(exp)
    elif is_quote_string(exp):
        return exp[1:-1]

def is_variable(exp):
    return not is_pair(exp)

def set_variable(var, value, env):
    env[0][var] = value

def lookup_variable_value(var, env):
    for a_frame in env:
        if var in a_frame:
            return a_frame[var]
    print(env)
    raise Exception(f"{var} is not defined in the env")

def is_lambda(exp):
    return exp[0] == 'lambda'

def make_lambda(params, body):
    # a lambda expression contains prarms and body
    # params: ["arg1", ...] or []
    # body contains one or multiple exps
    # e.g: ["a", ["+", 1, 2], ...]
    if not body:
        raise Exception("body is empty -- make_lambda")
    lambda_exp = ['lambda']
    lambda_exp.append(params)
    for exp in body:
        lambda_exp.append(exp)
    return lambda_exp

def lambda_parameters(exp):
    return exp[1]

def lambda_body(exp):
    return exp[2:]

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

def defination_variable(exp):
    if is_pair(exp[1]):
        return exp[1][0]
    else:
        return exp[1]

def defination_value(exp):
    if is_pair(exp[1]):
        params = exp[1][1:]
        body = exp[2:]
        return make_lambda(params, body)
    else:
        return exp[2]

def eval_defination(exp, env):
    # (define a 42)
    # (define (f arg1 arg2) body)
    set_variable(
        defination_variable(exp),
        eval_(defination_value(exp), env),
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

def is_if(exp):
    return exp[0] == 'if'

def eval_if(exp, env):
    if eval_(if_predicate(exp), env) is True:
        return eval_(if_consequent(exp), env)
    else:
        return eval_(if_alternative(exp), env)

def if_predicate(exp):
    return exp[1]

def if_consequent(exp):
    return exp[2]

def if_alternative(exp):
    return exp[3]

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
        assert isinstance(exp, list)
        ret = eval_(exp, env)
    return ret

def extend_environment(params, vals, env=[]):
    assert len(params) == len(vals)
    assert isinstance(env, list)

    new_env = dict(zip(params, vals))
    return [new_env] + env


def list_impl(*args):
    if not args:
        return None
    return (args[0], list_impl(*args[1:]))

ENV = extend_environment(
    ['+', '-', '*', '/', '=', '<', '>', 'display', 'cons', 'car', 'cdr', 'null', 'null?', 'true', 'false', 'list', 'abs'],
    [op.add,
     op.sub,
     op.mul,
     op.truediv,
     op.eq,
     op.lt,
     op.gt,
     print,
     lambda x, y: (x, y),
     lambda x: x[0],
     lambda x: x[1],
     None,
     lambda x: False if x else True,
     True,
     False,
     list_impl,
     abs,
    ]
)
