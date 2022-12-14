from os import walk

from lisp_evaluator import eval_, parse, ENV

test_dir = './test-cases'
filenames = next(walk(test_dir), (None, None, []))[2]  # [] if no file
filenames = [e for e in filenames if e.endswith('.ss')]

def get_blocks(lines):
    """A code block contains one line or multiple lines."""
    def balanced_parentheses(line):
        return line.count('(') == line.count(')')

    splits = lines.split('\n')
    blocks = []
    string = ''
    for line in splits:
        if not line:
            continue
        if line.startswith(';'):
            continue
        string = string + line
        if balanced_parentheses(string):
            blocks.append(string)
            string = ''
    return blocks

for fn in filenames:
    test_file = test_dir + '/' + fn
    with open(test_file, 'r') as f:
        print(f'-------------------- {test_file} --------------------')
        blocks = get_blocks(f.read())
        for block in blocks:
            result = eval_(
                parse(block),
                ENV
            )
            if result is False:
                print(f'  Failed: {block}')
                assert False
            elif result is True:
                print(f'  Passed: {block}')
