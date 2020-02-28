def is_even(exp):
    return exp % 2 == 0

def expmod(base, exp, m):
    if exp == 0:
        return 1

    if is_even(exp):
        return (expmod(base, exp / 2, m) ** 2) % m
    else:
        return (base * expmod(base, exp - 1, m)) % m

def slow_expmod(base, exp, m):
    return base ** exp % m

if __name__ == '__main__':
    import time
    t1 = time.time()
    print(f"fast: {expmod(3, 10000000, 26)}, time took: {time.time() - t1}")

    t1 = time.time()
    print(f"slow: {slow_expmod(3, 10000000, 26)}, time took: {time.time() - t1}")

    t1 = time.time()
    print(f"build-in: {pow(3, 10000000, 26)}, time took: {time.time() - t1}")
