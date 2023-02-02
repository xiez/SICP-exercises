# A register machine in Python with type annotations.
from abc import ABC
from typing import List, Any, Tuple, Callable
import operator as op

# utils --------------------
def is_tagged_list(pair: List, tag: str):
    assert isinstance(pair, list), pair
    assert isinstance(pair[0], str), pair
    return pair[0] == tag

def is_operation_exp(exp: List):
    """
    e.g.
     [['op', '-'], ['reg', 'counter'], ['const', 1]]
    """
    assert isinstance(exp, list)
    return is_tagged_list(exp[0][0], 'op')

def operation_exp_op(exp: List) -> str:
    """
    """
    return exp[0][1]

def operation_exp_operands(exp: List) -> List:
    return exp[1:]

# --------------------

class Instruction:
    """A machine instruction simply pairs the instruction text with the corresponding
    execution procedure.
    The instruction text is not used by our simulator, but it is handy to keep around
    for debugging.

    e.g.
        (
          ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
          <proc>
        )
    """
    def __init__(self, text: List, proc=None):
        self.text = text
        self.execution_proc = proc

    def set_execution_proc(self, proc):
        self.execution_proc = proc


class LabelTable:
    """A label table used to keep track of the labels with the corresponding instructions.

    e.g.
    expt-loop -> [
      (['test', ['op', '='], ['reg', 'counter'], ['const', 0]], <proc>),
      (['branch', ['label', 'expt-done']], <proc>),
      (['assign', 'counter', ['op', '-'], ['reg', 'counter'], ['const', 1]], <proc>),
      (['assign', 'product', ['op', '*'], ['reg', 'b'], ['reg', 'product']], <proc>),
    ]
    expt-done -> []
    """
    def __init__(self):
        self.dic = {}

    def lookup_label(self, label_name):
        try:
            self.dic[label_name]
        except KeyError:
            raise Exception(f"Undefined label: {label_name}")

    def insert_entry(self, label_name, instructions):
        if label_name in self.dic:
            raise Exception(f"Duplicated label: {label_name}")
        self.dic[label_name] = instructions

    def __str__(self):
        entries = []
        for k, v in self.dic.items():
            entries.append(f"{k} -> {v}")
        return "\n".join(entries)

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
    """A stack used to save register contents or labels."""
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


class BaseMachine(ABC):
    """A basic machine contains general compoments(
    a stack, pc and flag registers,
    )
    """
    def __init__(self):
        self._pc = Register('pc')  # program counter
        self._flag = Register('flag')
        self.stack = Stack()
        self.instruction_sequence = []
        self.operations = [
            ['initialize-stack', self.stack.initialize]
        ]
        self.register_table = {
            'pc': self._pc,
            'flag': self._flag,
        }

    def start(self):
        self._pc.set_contents(self.instruction_sequence)
        self.execute()

    def _instruction_execution_proc(self, inst):
        instruction = self.instruction_sequence.pop(0)

    def advance_pc(self):
        """Advance the instruction sequence by one."""
        self._pc.get_contents().pop(0)

    def execute(self):
        """Get an instruction, executes it by calling the instruction
        execution procedure, and repeats this cycle until there are no
        more instructions.
        """
        insts = self._pc.get_contents()
        print(f'execute insts: {insts[:1]} ... ')
        if not insts:
            print('done')
            return

        # each machine instruction is a data structure that includes a
        # procedure of no arguments, called the instruction execution
        # procedure, such that calling this procedure simulates executing the instruction
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

    def assemble(self, controller_text: List) -> List[Instruction]:
        """Transform controller text into a corresponding list of instructions,
        each with its execution procedure.

        e.g.
        ['test', ['op', '='], ['reg', 'counter'], ['const', 0]] ->
        (
          ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
          <proc>
        )

        """
        print(f'Assembling controller text: {controller_text}')

        initial_inst_list, label_table = self._extract_labels(controller_text)
        # print(initial_inst_list)
        # print('========')
        # print(label_table)
        inst_list_with_proc = self._update_insts(initial_inst_list, label_table)
        return inst_list_with_proc

    def _extract_labels(self, controller_text: List) -> Tuple[List, LabelTable]:
        """Build the initial instruction list [<text>, ...],
        and label table [(<label_name>, <insts>), ...].

        e.g.
        instruction list: [
          ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
          ...
        ]

        label table: [
        (
          'expt-loop',
          [
            ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
            ['branch', ['label', 'expt-done']],
            ['assign', 'counter', ['op', '-'], ['reg', 'counter'], ['const', 1]],
          ],
        ),
        ...
        ]
        """
        insts = []
        label_table = LabelTable()
        for idx, a_inst in enumerate(controller_text):
            if isinstance(a_inst, list):
                insts.append(a_inst)
            elif isinstance(a_inst, str):
                label_table.insert_entry(a_inst, controller_text[idx+1:])
            else:
                raise Exception(f"Invalid instruction text {a_inst}")
        return insts, label_table

    def _update_insts(self, insts: List, labels: LabelTable) -> List[Instruction]:
        """Modifies the instruction list *insts*, which initially contains only the text of the
        instrcutions, to include the corresponding execution procedures.
        """
        insts_with_proc = []
        for a_inst in insts:
            inst = self._make_execution_procedure(
                a_inst, labels
            )
            insts_with_proc.append(inst)
        return insts_with_proc

    def _make_execution_procedure(self, inst: List, labels: LabelTable) -> Instruction:
        assert isinstance(inst, list)

        inst_type = inst[0]

        if inst_type == 'test':
            ...
        elif inst_type == 'branch':
            ...
        elif inst_type == 'assign':
            proc = self._make_assign(inst, labels)
            return Instruction(inst, proc)
        elif inst_type == 'goto':
            ...
        else:
            assert False, f"TODO: {inst_type}"

    def _make_assign(self, inst: List, labels) -> Callable:
        assign_reg_name = inst[1]
        target = self.get_register(assign_reg_name)
        value_exp = inst[2:]

        if is_operation_exp(value_exp):
            value_proc = make_operation_exp(value_exp, self, labels)
        else:
            value_proc = make_primitive_exp(value_exp, self, labels)

        def proc():
            target.set_contents(value_proc())
            self.advance_pc()
        return proc


class Machine(BaseMachine):
    """A general purpose register machine."""
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


# utils ----------
def make_operation_exp(exp: List, machine: Machine, labels: LabelTable) -> Callable:
    """
    exp: operation exp, e.g. [['op', '*'], ['reg', 'b'], ['reg', 'product']]
    """
    op = machine.lookup_prim(operation_exp_op(exp))
    aprocs = [make_primitive_exp(o, machine, labels) for o in operation_exp_operands]

    def f():
        args = [p() for p in aprocs]
        return op(args)
    return f

is_constant_exp = lambda exp: is_tagged_list(exp, 'const')
is_label_exp = lambda exp: is_tagged_list(exp, 'label')
is_register_exp = lambda exp: is_tagged_list(exp, 'register')
constant_exp_value = register_exp_value = label_exp_value = lambda exp: exp[1]

def make_primitive_exp(exp: List, machine: Machine, labels: LabelTable) -> Callable:
    """
    exp: primivte exp, e.g. ['const', 1], ['label', 'expt-loop'], ['reg', 'counter']
    """
    if is_constant_exp(exp):
        ...
    elif is_label_exp(exp):
        ...
    elif is_register_exp(exp):
        ...
    else:
        raise Exception(f"Unknown exression type: {exp}")

# -----------------------

# start a machine
'''
(define expt-machine-iter
  (make-machine
   '(b counter product)
   (list (list '= =) (list '- -) (list '* *))
   '(expt-loop
       (test (op =) (reg counter) (const 0))
       (branch (label expt-done))
       (assign counter (op -) (reg counter) (const 1)) ;counter = counter - 1
       (assign product (op *) (reg b) (reg product)) ;product = product * b
       (goto (label expt-loop))
     expt-done)))
'''
regs = ['b', 'counter', 'product']
ops = [
    ['=', op.eq],
    ['-', op.sub],
    ['*', op.mul],
]
controller_text = [
    'expt-loop',
    ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
    ['branch', ['label', 'expt-done']],
    ['assign', 'counter', ['op', '-'], ['reg', 'counter'], ['const', 1]],
    ['assign', 'product', ['op', '*'], ['reg', 'b'], ['reg', 'product']],
    ['goto', ['label', 'expt-loop']],
    'expt-done'
]

expt_machine = Machine(regs, ops, controller_text)
expt_machine.start()
print(expt_machine.get_register('product').get_contents())
