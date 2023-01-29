# A register machine in Python with type annotations.
from abc import ABC
from typing import List, Any


class BaseMachine(ABC):
    """A basic machine contains general compoments(
    a stack, pc and flag registers,
    )
    """
    def __init__(self):
        self._pc = Register('pc')  # program counter
        _flag = Register('flag')
        self.stack = Stack()
        self.instruction_sequence = []
        self.operations = [
            ['initialize-stack', self.stack.initialize]
        ]
        self.register_table = {
            'pc': self._pc,
            'flag': _flag,
        }

    def start(self):
        self._pc.set_contents(self.instruction_sequence)
        self.execute()

    def _instruction_execution_proc(self, inst):
        # TODO
        self.instruction_sequence.pop(0)

    def execute(self):
        insts = self._pc.get_contents()
        print(f'execute insts: {insts[:1]} ... ')
        if not insts:
            print('done')
            return

        self._instruction_execution_proc(insts[0])
        self.execute()

    def install_instruction_sequence(self, seq: List):
        self.instruction_sequence += seq

    def allocate_register(self, name):
        """Allocate a new register to the register table."""
        if name in self.register_table:
            raise Exception(f"Multiply defined register: {name}")
        self.register_table[name] = Register(name)

    def get_register(self, name):
        try:
            return self.register_table[name]
        except KeyError:
            raise Exception(f"Unknown register: {name}")

    def install_operations(self, ops: List):
        self.operations += ops

    def assemble(self, controller_text):
        print(f'Assembling controller text: {controller_text}')
        return controller_text

class Machine(BaseMachine):
    def __init__(
            self,
            register_names: List[str],
            ops: List,
            controller_text: str,
    ):
        super().__init__()

        for rn in register_names:
            self.allocate_register(rn)
        self.install_operations(ops)
        self.install_instruction_sequence(
            self.assemble(controller_text)
        )

class Register:
    """A machine register used to save values or labels."""
    def __init__(
            self,
            name: str,
    ):
        self.name = name
        self.contents = '*unassigned*'

    def get_contents(self):
        return self.contents

    def set_contents(self, value):
        self.contents = value

class Stack:
    """A stack to save register contents (values or labels)"""
    def __init__(self):
        self.lst = None

    def initialize(self):
        self.lst = []

    def push(self, x: Any):
        assert self.lst, "Stack not initialized."
        self.lst.append(x)

    def pop(self):
        assert self.lst, "Stack not initialized."
        try:
            self.lst.pop(0)
        except IndexError:
            raise Exception("Empty stack: POP")

# start a machine

'''
(expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label expt-base))
 (save n)
 (save continue)
 (assign n (op -) (reg n) (cons 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 expt-base
 (assign val (const 1))
 (goto (reg continue))
 after-expt
 (restore continue)
 (restore n)
 (assign val (op *) (reg b) (reg val))
 (goto (reg continue))
 expt-done)
'''
controller_text = [
    'expt-loop',
    ['test', ['op', '='], ['reg', 'n'], ['const', 0]],
    ['branch', ['label', 'expt-base']],
    ['save', 'n'],
    ['save', 'continue'],
    ['assign', 'n', ['op', '-'], ['reg', 'n'], ['cons', 1]],
    ['assign', 'continue', ['label', 'after-expt']],
    ['goto', ['label', 'expt-loop']],
    'expt-base',
    ['assign', 'val', ['const', 1]],
    ['goto', ['reg', 'continue']],
    'after-expt',
    ['restore', 'continue'],
    ['restore', 'n'],
    ['assign', 'val', ['op', '*'], ['reg', 'b'], ['reg', 'val']],
    ['goto', ['reg', 'continue']],
    'expt-done'
]

m = Machine(['a', 'b'], ['op1', 'op2', 'op3'], controller_text)
m.start()
