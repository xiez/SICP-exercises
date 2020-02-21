# f(0) = 0, f(1) = 1, f(2) = 1,
# f(n) = f(n-1) + f(n-2)

def fib(n):
    if n == 0:
        return 0

    elif n == 1:
        return 1

    else:
        return fib(n-1) + fib(n-2)

def fib2(n):
    a = 1
    b = 0

    if n == 0:
        return b
    if n == 1:
        return a

    for _ in range(n):
        print(f'#{_}')
        tmp = a
        a = a + b
        b = tmp
        # print(a)
        # print(b)
    return b

# def fib3(n):
#     def fib_iter(a, b, count):
#         return b if count == 0 else fib_iter(a+b, a, (count-1))

#     return fib_iter(1, 0, n)

def fib4(n):
    a, b = 0, 1

    for _ in range(n):
        tmp = b
        b = a + b
        a = tmp
    return a


# print(fib(10))
# print('=========')
# # print(fib2(30))
# # print(fib3(10))
# for i in range(10):
#     print(fib4(i))

def f(n):
    if n < 3:
        return n
    else:
        return f(n-1) + 2 * f(n-2) + 3 * f(n-3)

def f2(n):
    a, b, c = 0, 1, 2

    for _ in range(n):
        t1 = c
        t2 = b
        c = c + 2 * b + 3 * a
        b = t1
        a = t2

    return a

for i in range(10):
    print(f'i: {i}, {f(i)}')

for i in range(10):
    print(f'i: {i}, {f2(i)}')
