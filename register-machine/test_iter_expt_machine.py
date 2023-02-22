import operator as op

from machine import Machine
from utils import get_controller_text

# exer 5.4: iterative expt machine
regs = ["b", "counter", "product"]
ops = [
    ["=", op.eq],
    ["-", op.sub],
    ["*", op.mul],
]
controller_text = get_controller_text(__file__)

expt_machine = Machine(regs, ops, controller_text, name='iterative expt machine')
expt_machine.start()
res = expt_machine.get_register("product").get_contents()
print("2 ** 10: ", res)
assert res == 1024
