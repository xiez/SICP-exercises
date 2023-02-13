# tokenize & parse --------------------
def tokenize(lines):
    def _tokenize(line):
        splits = line.replace("(", "( ").replace(")", " )").split(" ")
        return [s for s in splits if s]

    ret = []
    for line in lines.split("\n"):
        line = line.strip()
        if not line:
            continue
        if line.startswith(';'):
            continue
        if ';' in line:
            line = line.split(';')[0]
        ret += _tokenize(line)
    return ret


def analyze(tokens):
    def expr():
        assert tokens.pop(0) == "("  # remove (
        op = operator()
        args = operands()
        assert tokens.pop(0) == ")"  # remove )
        return [op] + args

    def operator():
        try:
            if tokens[0] == "(":
                return expr()
            else:
                return tokens.pop(0)
        except IndexError:
            raise SyntaxError("expected a ')' to close '('")

    def operands():
        if not tokens:
            return []
        ret = []
        try:
            while tokens[0] != ")":
                ret.append(operand())
            return ret
        except IndexError:
            raise SyntaxError("expected a ')' to close '('")

    def operand():
        if tokens[0] == "(":
            return expr()
        else:
            return analyze_operand(tokens.pop(0))

    ret = expr()
    if tokens:
        raise SyntaxError(f"unexpected tokens: {tokens}")
    return ret


def analyze_operand(token):
    try:
        return int(token)
    except (TypeError, ValueError):
        try:
            return float(token)
        except (TypeError, ValueError):
            return token


def parse(line):
    try:
        return analyze(tokenize(line))
    except:
        return tokenize(line)[0]
