import operator as op

from machine import Machine
from parser import parse

# exer5.3: sqrt machine
controller_text = parse('''
(sqrt-loop
 (assign x (op read))
 (assign guess (const 1.0))
 test-guess
 (test (op good-enough?) (reg guess) (reg x))
 (branch (label done))
 (assign guess (op improve) (reg guess) (reg x))
 (goto (label test-guess))
 done
 (perform (op print) (reg guess)))
''')
regs = ['x', 'guess']
def is_good_enough(guess, val):
    ret = abs(guess ** 2 - val) < 0.001
    return ret

def improve(guess, val):
    return (guess + val / guess) / 2

ops = [
    ['good-enough?', is_good_enough],
    ['improve', improve],
    ['print', print],
    ['read', lambda: int(input('Enter a number: '))],
]

sqrt_machine = Machine(regs, ops, controller_text, name='sqrt machine')
sqrt_machine.start()
