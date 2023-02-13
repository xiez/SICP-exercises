import operator as op

from machine import Machine
from parser import parse

# exer 5.4: iterative expt machine
regs = ["b", "counter", "product"]
ops = [
    ["=", op.eq],
    ["-", op.sub],
    ["*", op.mul],
]
controller_text = parse('''
(controller
 (assign b (const 2))             ;base = 2
 (assign counter (const 10))             ;counter = n
 (assign product (const 1))             ;product = 1
 expt-loop
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
 (assign product (op *) (reg b) (reg product)) ;product = product * b
 (goto (label expt-loop))
 expt-done
 )
''')

expt_machine = Machine(regs, ops, controller_text, name='iterative expt machine')
expt_machine.start()
res = expt_machine.get_register("product").get_contents()
print("result: ", res)
assert res == 1024
