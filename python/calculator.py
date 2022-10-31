from typing import List

def tokenize(line):
    splits = line.replace('(', ' ( ').replace(')', ' ) ').replace(',', ' , ').split(' ')
    return [s for s in splits if s]

def analyze_token(a_token):
    try:
        return int(a_token)
    except ValueError:
        try:
            return float(a_token)
        except ValueError:
            return a_token

def analyze_operands(tokens: List) -> List:
    operands = []
    while True:
        a_token = tokens[0]
        if a_token == ')':
            break
        if a_token == ',':
            tokens.pop(0)
            continue
        operands.append(analyze(tokens))
    tokens.pop(0)               # remove )
    return operands

def analyze(tokens):
    # print(f'analyze tokens: {tokens}')
    token = analyze_token(tokens.pop(0))
    if type(token) in (int, float):
        return token
    else:
        tokens.pop(0)           # remove (
        return [
            token,
            analyze_operands(tokens)
        ]

def parse(line):
    tokens = tokenize(line)
    ast = analyze(tokens)
    return ast

line = 'add(2, mul(4, 6))'
# print(tokenize(line))
print(parse(line))

line = 'add(2, mul(4, 6), sub(9, 1))'
print(parse(line))

line = 'abs(-1)'
print(parse(line))

line = 'add(1, 1.1)'
print(parse(line))

line = '3.14'
print(parse(line))
