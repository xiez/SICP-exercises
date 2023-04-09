from typing import List, Any, Tuple, Callable, Dict

import lispy_ops as OP

class InstructionSequence:
    def __init__(self, needs=set(), modifies=set(), statements=[]):
        self.needs = needs
        self.modifies = modifies
        self.statements = statements

    def __add__(self, other):
        needs = self.needs.union(
            other.needs - self.modifies
        )
        modifies = self.modifies.union(other.modifies)
        statements = self.statements + other.statements
        return InstructionSequence(needs, modifies, statements)

    def __str__(self):
        return f"needs={self.needs},modifies={self.modifies},statements={self.statements}"


def compile(exp, target, linkage):
    if OP.is_self_evaluating(exp):
        return compile_self_evaluating(exp, target, linkage)

def compile_self_evaluating(exp, target, linkage):
    return end_with_linkage(
        linkage,
        InstructionSequence(
            modifies=set({target}),
            statements=[['assign', target, ['const', exp]]]
        )
    )

def end_with_linkage(linkage, inst_seq):
    return preserving(
        ['continue'],
        inst_seq,
        compile_linkage(linkage)
    )


def preserving(regs: List, seq1, seq2):
    if not regs:
        return seq1 + seq2
    else:
        first_reg = regs[0]
        if first_reg in seq2.needs and first_reg in seq1.modifies:
            return preserving(regs[1:], seq1, seq2)
        else:
            return preserving(regs[1:], seq1, seq2)


def compile_linkage(linkage):
    if linkage == 'return':
        return InstructionSequence(
            needs=set(['continue']),
            statements=[['goto', ['reg', 'continue']]]
        )
    elif linkage == 'next':
        return InstructionSequence(
            statements=InstructionSequence()
        )
    else:
        return InstructionSequence(statements=[['goto'], ['label', linkage]])

# test compile
print(compile(42, 'val', 'done'))
