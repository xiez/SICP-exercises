import operator as op

from machine import Machine
from utils import get_controller_text

regs = ['a', 'b', 'continue', 'retval']
controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['=', op.eq],
    ["*", op.mul],
    ['<', op.lt],
    ['-', op.sub],
]

machine = Machine(regs, ops, controller_text, name='gcd machine')
machine.start()
res = machine.get_register("retval").get_contents()
print("gcd(42,6): ", res)
assert res == 6
