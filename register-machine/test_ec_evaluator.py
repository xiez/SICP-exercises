import operator as op

from machine import Machine
from utils import get_controller_text
import lispy as lispy_ops

controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['=', op.eq],
    ["*", op.mul],
    ['<', op.lt],
    ['+', op.add],
    ['-', op.sub],

    ['self-evaluating?', lispy_ops.is_self_evaluating],
    ['variable?', lispy_ops.is_variable],
    ['lookup-variable-value', lispy_ops.lookup_variable_value],
    ['lambda?', lispy_ops.is_lambda],
    ['lambda-parameters', lispy_ops.lambda_parameters],
    ['lambda-body', lispy_ops.lambda_body],
    ['make-procedure', lispy_ops.make_compound_procedure],
    ['application?', lispy_ops.is_combination],
]

machine = Machine(ops, controller_text, name='EC-evaluator')

# machine.get_register('exp').set_contents(42)
# machine.get_register('exp').set_contents(3.14)
# machine.get_register('exp').set_contents('"hello"')

# machine.get_register('env').set_contents([{'foo': 'bar'}])
# machine.get_register('exp').set_contents('foo')

# lambda_exp = lispy_ops.parse('''
# (lambda (x y) (+ x y))
# ''')
# machine.get_register('exp').set_contents(lambda_exp)

lambda_apply = lispy_ops.parse('''
((lambda (x y) (+ x y)) 1 2)
''')
machine.get_register('exp').set_contents(lambda_apply)

machine.start()

# res = machine.get_register("val").get_contents()
# print(res)
