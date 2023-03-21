# pair
class _Pair:
    def __init__(self, x, y):
        self.car = x
        self.cdr = y

    def __eq__(self, other):
        if not isinstance(other, _Pair):
            return False
        return True if self.car == other.car and self.cdr == other.cdr else False

    def __str__(self):
        return f'(car={self.car},cdr={self.cdr})'

def is_pair(x):
    return isinstance(x, _Pair)
def car(x):
    assert is_pair(x)
    return x.car
def cdr(x):
    assert is_pair(x)
    return x.cdr
def cons(x, y):
    return _Pair(x, y)
def set_cdr(x, y):
    x.cdr = y

#################### test
c = cons(3, cons(2, cons(1, None)))
# print(c)
assert cdr(c) == cons(2, cons(1, None))
cddr = cdr(cdr(c))
assert cddr == cons(1, None)
assert bool(cddr) is True
cdddr = cdr(cddr)
assert cdddr is None
assert bool(cdddr) is False
assert c == cons(3, cons(2, cons(1, None)))

c2 = cons(0, cons(-1, None))
set_cdr(cddr, c2)
assert c == cons(3, cons(2, cons(1, cons(0, cons(-1, None)))))
