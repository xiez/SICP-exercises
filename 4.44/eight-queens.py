# a naive (but optimal) implementation of eight queens
def distinct(lst):
    return True if len(set(lst)) == len(lst) else False

def row_attack(*queens):
    return False if distinct(queens) else True

def cross_attack(*queens):
    def cross_attack_any(queen, queens):
        for idx, a_queen in enumerate(queens):
            if abs(a_queen - queen) == 1 + idx:
                # if current queen placed at row 1,
                # next column should not be placed at row 2
                # next next column should not be placed at row 3
                # ...
                return True
        return False
    if len(queens) == 0:
        return False
    elif len(queens) == 1:
        return False
    else:
        first = queens[0]
        rest = queens[1:]
        return True if cross_attack_any(first, rest) else cross_attack(*rest)

def attack(*queens):
    return True if row_attack(*queens) or cross_attack(*queens) else False

def eight_queens():
    count = 0
    q1, q2, q3, q4, q5, q6, q7, q8 = 1, 1, 1, 1, 1, 1, 1, 1
    while q1 <= 8:
        while q2 <= 8:
            if attack(q1, q2):
                q2 += 1
                continue
            while q3 <= 8:
                if attack(q1, q2, q3):
                    q3 += 1
                    continue
                while q4 <= 8:
                    if attack(q1, q2, q3, q4):
                        q4 += 1
                        continue
                    while q5 <= 8:
                        if attack(q1, q2, q3, q4, q5):
                            q5 += 1
                            continue
                        while q6 <= 8:
                            if attack(q1, q2, q3, q4, q5, q6):
                                q6 += 1
                                continue
                            while q7 <= 8:
                                if attack(q1, q2, q3, q4, q5, q6, q7):
                                    q7 += 1
                                    continue
                                while q8 <= 8:
                                    if attack(q1, q2, q3, q4, q5, q6, q7, q8):
                                        q8 += 1
                                        continue
                                    print('rows for queen at column 1-8: ')
                                    print(q1, q2, q3, q4, q5, q6, q7, q8)
                                    q8 += 1
                                    count += 1
                                q7 += 1
                                q8 = 1
                            q6 += 1
                            q7, q8 = 1, 1
                        q5 += 1
                        q6, q7, q8 = 1, 1, 1
                    q4 += 1
                    q5, q6, q7, q8 = 1, 1, 1, 1
                q3 += 1
                q4, q5, q6, q7, q8 = 1, 1, 1, 1, 1
            q2 += 1
            q3, q4, q5, q6, q7, q8 = 1, 1, 1, 1, 1, 1
        q1 += 1
        q2, q3, q4, q5, q6, q7, q8 = 1, 1, 1, 1, 1, 1, 1
    print(f'total count: {count}')

eight_queens()
