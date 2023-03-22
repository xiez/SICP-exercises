import operator as op

from machine import Machine
from utils import get_controller_text
from pair import cons, car, cdr, is_pair
from vector import make_vector, vector_ref, vector_set
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
    ['make-vector', make_vector],
    ['vector-ref', vector_ref],
    ['vector-set!', vector_set],
]

machine = Machine(ops, controller_text, name='append machine with vector')
machine.start()

# address of x is 2
assert machine.get_register("x").get_contents() == 2
res = machine.get_register("retval").get_contents()
print("append (4 3) (2 1), &res: ", res)
assert res == 2                 # same as x

free = machine.get_register("free").get_contents()
print("& of free: ", free)
assert res == 5
