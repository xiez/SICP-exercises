import operator as op

from machine import Machine
from utils import get_controller_text

regs = ['n', 'tmp', 'continue', 'retval']
controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['=', op.eq],
    ["*", op.mul],
    ['<', op.lt],
    ['+', op.add],
    ['-', op.sub],
]

machine = Machine(ops, controller_text, name='fib machine2')
machine.start()
res = machine.get_register("retval").get_contents()
print("fib(8): ", res)
assert res == 21
