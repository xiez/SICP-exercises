# vector
import array

def make_vector(capacity=10, type_code='i'):
    vec = array.array(type_code, [0] * capacity)
    return vec

def vector_ref(vec, n):
    return vec[n]

def vector_set(vec, n, val):
    vec[n] = val

# test
vec = make_vector()
vector_set(vec, 1, 10)
assert vector_ref(vec, 0) == 0
assert vector_ref(vec, 1) == 10
