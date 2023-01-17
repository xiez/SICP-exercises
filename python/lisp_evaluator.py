# A Lisp evaluator implemented in Python
# Note: before run this script, increase stack size limit by: ulimit -s 60000
import operator as op

import sys
sys.setrecursionlimit(10**6)


# utils --------------------
def is_list(exp):
    return True if isinstance(exp, list) else False


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


def is_double_quote_string(exp):
    return True if exp[0] == '"' else False


# tokenize & parse --------------------
def tokenize(lines):
    def _tokenize(line):
        splits = line.replace("(", "( ").replace(")", " )").split(" ")
        return [s for s in splits if s]

    ret = []
    for line in lines.split("\n"):
        if not line.strip():
            continue
        ret += _tokenize(line.strip())
    return ret


def analyze(tokens):
    def expr():
        assert tokens.pop(0) == "("  # remove (
        op = operator()
        args = operands()
        assert tokens.pop(0) == ")"  # remove )
        return [op] + args

    def operator():
        try:
            if tokens[0] == "(":
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
            while tokens[0] != ")":
                ret.append(operand())
            return ret
        except IndexError:
            raise SyntaxError("expected a ')' to close '('")

    def operand():
        if tokens[0] == "(":
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
    """Evaluate an expression within an environment."""
    if is_self_evaluating(exp):  # 42, 3.14, "hello"
        return self_evaluating(exp)
    elif is_variable(exp):  # foo + - * / null
        return lookup_variable_value(exp, env)
    # elif is_quote(exp):
    #     return get_quote_text(exp)
    elif is_if(exp):
        return eval_if(exp, env)
    elif is_cond(exp):
        return eval_if(cond_to_if(exp), env)
    elif is_lambda(exp):
        return make_compound_procedure(
            lambda_parameters(exp),
            lambda_body(exp),
            env,
        )
    elif is_definination(exp):
        return eval_defination(exp, env)
    elif is_combination(exp):  # ['+', 1, 1]
        return apply_(
            eval_(get_combination_operator(exp), env),
            list_of_values(get_combination_operands(exp), env),
        )
    else:
        raise Exception(f"Unknown expression type -- EVAL {exp}")


def apply_(proc, args):
    """Apply procedure with actual arguments."""
    if is_primitive_proc(proc):
        return apply_in_python(proc, args)
    elif is_compound_procedure(proc):
        return eval_sequence(
            procedure_body(proc),
            extend_environment(
                procedure_params(proc), args, procedure_environment(proc)
            ),
        )
    else:
        raise Exception(f"Unknown procedure type -- APPLY {proc}")


def is_self_evaluating(exp):
    if is_list(exp):
        return False

    if isinstance(exp, int) or isinstance(exp, float):
        return True

    return True if is_number_string(exp) or is_double_quote_string(exp) else False


def self_evaluating(exp):
    assert not is_list(exp)

    if isinstance(exp, int) or isinstance(exp, float):
        return exp
    if is_number_string(exp):
        try:
            return int(exp)
        except:
            return float(exp)
    elif is_double_quote_string(exp):
        return exp[1:-1]


def is_variable(exp):
    return not is_list(exp)


def set_variable(var, value, env):
    env[0][var] = value


def lookup_variable_value(var, env):
    for a_frame in env:
        if var in a_frame:
            return a_frame[var]
    print(env)
    raise Exception(f"Error: {var} is not defined in the ENV.")


def is_lambda(exp):
    return exp[0] == "lambda"


def make_lambda(params, body):
    # a lambda expression contains prarms and body
    # params: ["arg1", ...] or []
    # body contains one or multiple exps
    # e.g: ["a", ["+", 1, 2], ...]
    if not body:
        raise Exception("body is empty -- make_lambda")
    lambda_exp = ["lambda"]
    lambda_exp.append(params)
    for exp in body:
        lambda_exp.append(exp)
    return lambda_exp


def lambda_parameters(exp):
    return exp[1]


def lambda_body(exp):
    return exp[2:]


def is_compound_procedure(exp):
    return exp[0] == "procedure"


def make_compound_procedure(params, body, env):
    """Make a procedure with formal arguments, body and environment."""
    return ["procedure", params, body, env]


def procedure_params(exp):
    return exp[1]


def procedure_body(exp):
    return exp[2]


def procedure_environment(exp):
    return exp[3]


def is_definination(exp):
    return exp[0] == "define"


def defination_variable(exp):
    if is_list(exp[1]):
        return exp[1][0]
    else:
        return exp[1]


def defination_value(exp):
    if is_list(exp[1]):
        params = exp[1][1:]
        body = exp[2:]
        return make_lambda(params, body)
    else:
        return exp[2]


def eval_defination(exp, env):
    # (define a 42)
    # (define (f arg1 arg2) body)
    set_variable(defination_variable(exp), eval_(defination_value(exp), env), env)


# def is_quote(exp):
#     if not is_list(exp):
#         return False
#     splits = exp.split(' ')
#     if len(splits) > 1 and splits[0] == '(quote':
#         return True
#     return False

# def get_quote_text(exp):
#     assert is_quote(exp), exp
#     splits = exp.split(' ')
#     return trim_parenthese(''.join(splits[1:]))


def is_if(exp):
    return exp[0] == "if"


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


def make_if(predicate, consequent, alternative):
    return ["if", predicate, consequent, alternative]


def is_cond(exp):
    return exp[0] == "cond"


def cond_clauses(exp):
    return exp[1:]


def expand_clauses(clauses):
    # a_clause:
    # e.g. [['=', ['position-row', 'posn1'], ['position-row', 'posn2']], 'true']
    # e.g. ['else', 'true']
    assert len(clauses) > 1, clauses
    assert clauses[-1][0] == "else", clauses[-1]

    def is_else_clause(clause):
        return clause[0] == "else"

    def clause_predicate(clause):
        return clause[0]

    def clause_action(clause):
        return clause[1]

    def clauses_to_nested_if(clauses):
        if not clauses:
            return

        a_clause = clauses[0]
        if is_else_clause(a_clause):
            return clause_action(a_clause)
        else:
            return make_if(
                clause_predicate(a_clause),
                clause_action(a_clause),
                clauses_to_nested_if(clauses[1:]),
            )

    return clauses_to_nested_if(clauses)


def cond_to_if(exp):
    ret = expand_clauses(cond_clauses(exp))
    return ret


def is_combination(exp):
    return is_list(exp)


def get_combination_operator(exp):
    return exp[0]


def get_combination_operands(exp):
    return exp[1:]


def list_of_values(operands, env):
    return [eval_(o, env) for o in operands]


def is_primitive_proc(proc):
    return callable(proc)


def apply_in_python(proc, args):
    return proc(*args)


def eval_sequence(exps, env):
    # use last value of exps as the return value
    ret = None
    for exp in exps:
        assert isinstance(exp, list), exp
        ret = eval_(exp, env)
    return ret


def extend_environment(params, vals, env=[]):
    assert len(params) == len(vals), (params, vals)
    assert isinstance(env, list)
    new_env = dict(zip(params, vals))
    return [new_env] + env


def list_impl(*args):
    if len(args) == 0:
        return ()
    return (args[0], list_impl(*args[1:]))

def print_env():
    print(ENV)

ENV = extend_environment(
    [
        "+",
        "-",
        "*",
        "/",
        "=",
        "<",
        ">",
        "display",
        "cons",
        "car",
        "cdr",
        "null",
        "null?",
        "true",
        "false",
        "list",
        "abs",
        "print-env",
    ],
    [
        op.add,
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
        (),
        lambda x: False if x else True,
        True,
        False,
        list_impl,
        abs,
        print_env,
    ],
)

# REPL
def repl():
    """read eval print loop"""
    print("A toy lisp evaluater.")
    while 1:
        input_str = read_multiple_lines().strip()
        if not input_str:
            continue
        if input_str.startswith(";"):
            continue

        try:
            res = eval_(parse(input_str), ENV)
            print(res)
        except Exception as e:
            print(e)


def read_multiple_lines():
    def is_balanced_parentheses(line):
        stack = []
        for char in line:
            if not char or char not in ["(", ")"]:
                continue
            if not stack:
                stack.append(char)
                continue
            if char == ")" and stack[-1] == "(":
                stack.pop()
                continue
            stack.append(char)
        return False if "(" in stack else True

    lines = []
    line = input("** >> ")
    if is_balanced_parentheses(line):
        return line

    lines.append(line)
    while 1:
        line = input("..... ").strip()
        if not line:
            continue
        if line.startswith(";"):  # ignore lines startswith ;
            continue
        lines.append(line)
        if is_balanced_parentheses(" ".join(lines)):
            return " ".join(lines)


if __name__ == "__main__":
    repl()
