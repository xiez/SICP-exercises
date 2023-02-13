import operator as op

from machine import Machine
from parser import parse

# exer5.3: sqrt machine
controller_text = parse('''
(controller
 (assign x (op read))
 (assign guess (const 1.0))

 sqrt-loop
 (assign a (op square) (reg guess))     ;square is considered as a primitive op
 (assign a (op -) (reg a) (reg x))
 (assign a (op abs) (reg a))
 (test (op <) (reg a) (const 0.001))    ;if good-enough?, goto done with result in reg guess
 (branch (label done))
 (assign a (op /) (reg x) (reg guess))  ;otherwise, improve guess
 (assign guess (op average) (reg guess) (reg a))
 (goto (label sqrt-loop))

 done
 (perform (op print) (reg guess)))
''')

regs = ['a', 'x', 'guess']
ops = [
    ['print', print],
    ['read', lambda: int(input('Enter a number: '))],
    ['square', lambda x: x * x],
    ["-", op.sub],
    ["/", op.truediv],
    ["<", op.lt],
    ["abs", abs],
    ['average', lambda a, b: (a + b) / 2],
]

sqrt_machine = Machine(regs, ops, controller_text, name='sqrt machine2')
sqrt_machine.start()
