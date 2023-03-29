import operator as op

from machine import Machine
from utils import get_controller_text
import lispy as OP

controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['parse-read', lambda s: OP.parse(input(s))],
    ['=', op.eq],
    ["*", op.mul],
    ['<', op.lt],
    ['+', op.add],
    ['-', op.sub],

    ['self-evaluating?', OP.is_self_evaluating],
    ['variable?', OP.is_variable],
    ['lookup-variable-value', OP.lookup_variable_value],
    ['lambda?', OP.is_lambda],
    ['lambda-parameters', OP.lambda_parameters],
    ['lambda-body', OP.lambda_body],
    ['make-procedure', OP.make_compound_procedure],
    ['application?', OP.is_combination],
    ['combination-operands', OP.get_combination_operands],
    ['combination-operator', OP.get_combination_operator],
    ['initial-env', OP.initial_env],
    ['empty-arglist', lambda: []],
    ['no-operands?', lambda x: x == []],
    ['last-operand?', lambda x: len(x) == 1],
    ['first-operand', lambda x: x[0]],
    ['adjoin-arg', lambda x, lst: lst + [x]],
    ['rest-operands', lambda x: x[1:]],
    ['first-exp', lambda x: x[0]],
    ['last-exp?', lambda x: len(x) == 1],
    ['rest-exps', lambda x: x[1:]],
    ['primitive-procedure?', OP.is_primitive_proc],
    ['compound-procedure?', OP.is_compound_procedure],
    ['apply-primitive-procedure', OP.apply_in_python],
    ['procedure-parameters', OP.procedure_params],
    ['procedure-environment', OP.procedure_environment],
    ['extend-environment', OP.extend_environment],
    ['procedure-body', OP.procedure_body],
    ['definition?', OP.is_definition],
    ['definition-variable', OP.definition_variable],
    ['definition-value', OP.definition_value],
    ['set-variable-value!', OP.set_variable],
    ['if?', OP.is_if],
    ['if-predicate', OP.if_predicate],
    ['if-alternative', OP.if_alternative],
    ['if-consequent', OP.if_consequent],
    ['true?', lambda x: x is True],
    ['cond?', OP.is_cond],
    ['cond->if', OP.cond_to_if],
    ['let?', OP.is_let],
    ['let->combination', OP.let_to_combination],
    ['cond-clauses', OP.get_cond_clauses],
    ['first-clause', OP.first_clause],
    ['cond-else-clause?', OP.is_cond_else_clause],
    ['cond-predicate', OP.get_cond_predicate],
    ['rest-clauses', OP.rest_clauses],
    ['cond-actions', OP.get_cond_actions],
]

machine = Machine(ops, controller_text, name='EC-evaluator')
machine.start()

# machine.get_register('exp').set_contents(42)
# machine.get_register('exp').set_contents(3.14)
# machine.get_register('exp').set_contents('"hello"')

# machine.get_register('env').set_contents([{'foo': 'bar'}])
# machine.get_register('exp').set_contents('foo')

# lambda_exp = OP.parse('''
# (lambda (x y) (+ x y))
# ''')
# machine.get_register('exp').set_contents(lambda_exp)

# lambda_appl = OP.parse('''
# ((lambda (x y) (+ x y)) 1 2)
# ''')
# machine.get_register('exp').set_contents(lambda_appl)

# lambda_appl = OP.parse('''
# (((lambda (x) (lambda () (+ 1 x))) 2))
# ''')
# print(lambda_appl)
# assert False
# machine.get_register('exp').set_contents(lambda_appl)

# lambda_appl = OP.parse('''
# (define a 42)
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()
# lambda_appl = OP.parse('''
# (define b a)
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (define foo 42)
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()
# lambda_appl = OP.parse('''
# ((lambda (x y) (+ x y)) 1 foo)
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (if (= 1 1) "1==1" "1!=1")
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (if (= 1 2) "1==2" "1!=2")
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (if (= 1 1) (if (= 2 2) "1==1&2==2" "1==1&2!=2") "1!=1")
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (let ((a 1) (b 2)) (+ a b))
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (let ((a 1) (b 2)) (let ((c (+ a b))) (+ a (+ b c))))
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (cond ((= 1 1) "1==1")
#         ((= 1 2) "1==2")
#         (else "else.."))
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (cond ((= 1 2) "1==2")
#         ((= 2 2) "2==2")
#         (else "else.."))
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()

# lambda_appl = OP.parse('''
# (cond ((= 1 2) "1==2")
#         ((= 1 3) "1==3")
#         (else "else.."))
# ''')
# machine.get_register('exp').set_contents(lambda_appl)
# machine.start()


# start machine
# machine.start()
# res = machine.get_register("val").get_contents()
# print(res)
