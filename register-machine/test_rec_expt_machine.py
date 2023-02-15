import operator as op

from machine import Machine
from utils import get_controller_text

# exer 5.4: recursive expt machine
regs = ['continue', 'b', 'n', 'val']
ops = [
    ["=", op.eq],
    ["-", op.sub],
    ["*", op.mul],
]
controller_text = get_controller_text(__file__)

expt_machine = Machine(regs, ops, controller_text, name='recurive expt machine')
expt_machine.start()
res = expt_machine.get_register("val").get_contents()
print("result: ", res)
assert res == 1024
