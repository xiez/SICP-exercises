import operator as op

from machine import Machine
from utils import get_controller_text

regs = ['continue', 'n', 'val']
ops = [
    ["=", op.eq],
    ["-", op.sub],
    ["+", op.add],
    ["print", print],
]
controller_text = get_controller_text(__file__)

machine = Machine(regs, ops, controller_text, name='counter machine')
machine.start()
res = machine.get_register("val").get_contents()
print("1 + 2 + .. + 10: ", res)
assert res == 55
