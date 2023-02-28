import operator as op

from machine import Machine
from utils import get_controller_text

# exer5.3: sqrt machine
regs = ['a', 'x', 'guess']
controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['read', lambda: int(input('(Sqrt) Enter a number: '))],
    ['square', lambda x: x * x],
    ["-", op.sub],
    ["/", op.truediv],
    ["<", op.lt],
    ["abs", abs],
    ['average', lambda a, b: (a + b) / 2],
]

sqrt_machine = Machine(ops, controller_text, name='sqrt machine2')
sqrt_machine.start()
