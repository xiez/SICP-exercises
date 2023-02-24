import operator as op

from machine import Machine
from utils import get_controller_text

regs = ['n', 'val', 'continue']
controller_text = get_controller_text(__file__)
ops = [
    ['<', op.lt],
    ['-', op.sub],
    ['+', op.add],
]

fib_machine = Machine(ops, controller_text, name='fib machine')
fib_machine.start()

res = fib_machine.get_register('val').get_contents()
print(f"fib(8): {res}")
assert res == 21
