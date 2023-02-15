from machine import Machine
from utils import get_controller_text

# exer5.3: sqrt machine
regs = ['x', 'guess']
controller_text = get_controller_text(__file__)
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
