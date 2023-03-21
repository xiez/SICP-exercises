import operator as op

from machine import Machine
from utils import get_controller_text
from pair import cons, car, cdr, is_pair
# --------------------

controller_text = get_controller_text(__file__)
ops = [
    ['print', print],
    ['=', op.eq],
    ["*", op.mul],
    ['<', op.lt],
    ['+', op.add],
    ['-', op.sub],
    ['null?', lambda x: bool(x) is False],
    ['not', op.not_],
    ['pair?', is_pair],
    ['cons', cons],
    ['car', car],
    ['cdr', cdr],
]

machine = Machine(ops, controller_text, name='append machine')
machine.get_register('x').set_contents(
    cons(4, cons(3, None))
)
machine.get_register('y').set_contents(
    cons(2, cons(1, None))
)

machine.start()
res = machine.get_register("retval").get_contents()
print("append (4 3) (2 1): ", res)
assert res == cons(4, cons(3, cons(2, cons(1, None))))
