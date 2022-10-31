class Cons:
    def __init__(self, first, rest):
        self.first = first
        self.rest = rest

    def __str__(self):
        return f'cons({self.first} {self.rest})'

    def __eq__(self, other):
        return self.first == other.first and self.rest == other.rest

def cons(first, rest):
    return Cons(first, rest)

def first(cons):
    assert cons is not None
    return cons.first

def rest(cons):
    assert cons is not None
    return cons.rest

__all__ = ['cons', 'first', 'rest']

lst = cons(3, cons(2, cons(1, None)))
assert first(lst) == 3
assert rest(lst) == cons(2, cons(1, None))
