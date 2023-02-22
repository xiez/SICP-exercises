import operator as op

from machine import Machine
from utils import get_controller_text

regs = ['n', 'continue']
controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['read', lambda: int(input('Enter a number: '))],
    ["*", op.mul],
]

machine = Machine(regs, ops, controller_text, name='square machine')
machine.start()
res = machine.get_register("n").get_contents()
print("4 ** 2: ", res)
assert res == 16
