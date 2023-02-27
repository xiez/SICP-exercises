import operator as op

from machine import Machine
from utils import get_controller_text

controller_text = get_controller_text(__file__)
ops = [
    ['<', op.lt],
    ['-', op.sub],
    ['+', op.add],
    ['*', op.mul],
]

machine = Machine(ops, controller_text, name='fact machine')
machine.start()

res = machine.get_register('retval').get_contents()
print(f"factorial(10): {res}")
assert res == 3628800
