# A register machine in Python with type annotations.
from abc import ABC
from typing import List, Any, Tuple, Callable
import operator as op


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
        controller_text: ['test', ['op', '='], ['reg', 'counter'], ['const', 0]]

        instruction list with procs: [
        (
          ['test', ['op', '='], ['reg', 'counter'], ['const', 0]],
          <proc>
        ),
        ...
        ]

        """
        print(f'Assembling controller text: {controller_text}')

        initial_inst_list, label_table = self._extract_labels(controller_text)
        # print(initial_inst_list)
        # print('========')
        # print(label_table)
        inst_list_with_proc = self._update_insts(initial_inst_list, label_table)
        return inst_list_with_proc

    def _extract_labels(self, controller_text: List) -> Tuple[List, LabelTable]:
        """Build the initial instruction list and label table.

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
        """Include the corresponding execution procedures to the initial instruction list.

        TODO: use in-place updates.
        """
        insts_with_proc = []
        for a_inst in insts:
            inst = self._make_execution_procedure(
                a_inst, labels
            )
            insts_with_proc.append(inst)
        return insts_with_proc

    def _make_execution_procedure(self, inst: List, labels: LabelTable) -> Instruction:
        """Make corresponding instruction according to the instruction type.

        Same as *eval* in the meta-circular evaluator.
        """
        assert isinstance(inst, list)

        inst_type = inst[0]
        if inst_type == 'test':
            return Instruction(inst, self._make_test(inst, labels))
        elif inst_type == 'branch':
            return Instruction(inst, self._make_branch(inst, labels))
        elif inst_type == 'assign':
            return Instruction(inst, self._make_assign(inst, labels))
        elif inst_type == 'goto':
            return Instruction(inst, self._make_goto(inst, labels))
        else:
            assert False, f"TODO: {inst_type}"

    def _make_test(self, inst: List, labels: LabelTable) -> Callable:
        """Make a test instruction procedure."""
        condition_exp = test_condition(inst)
        if not is_operation_exp(condition_exp):
            raise Exception(f"Bad TEST instruction: {inst}")

        condition_proc = make_operation_exp(condition_exp, self, labels)

        def f():
            self._flag.set_contents(condition_proc())
            self.advance_pc()
        return f

    def _make_branch(self, inst: List, labels: LabelTable) -> Callable:
        """Make a branch instruction procedure based on the test flag."""
        dest_exp = branch_dest(inst)
        if not is_label_exp(dest_exp):
            raise Exception(f"Bad BRANCH instruction: {inst}")

        insts = labels.lookup_label(label_exp_label(dest_exp))

        def f():
            if self._flag.get_contents() is True:
                self._pc.set_contents(insts)
            else:
                self.advance_pc()
        return f

    def _make_assign(self, inst: List, labels: LabelTable) -> Callable:
        """Make an assignment instruction procedure."""
        target = self.get_register(assign_reg_name(inst))
        value_exp = assign_value_exp(inst)

        if is_operation_exp(value_exp):
            value_proc = make_operation_exp(value_exp, self, labels)
        else:
            value_proc = make_primitive_exp(value_exp, self, labels)

        def proc():
            target.set_contents(value_proc())
            self.advance_pc()
        return proc

    def _make_goto(self, inst: List, labels: LabelTable) -> Callable:
        """Make a goto instruction procedure.
        Controller can goto a label which points to instructions, or to
        a register which contains instructions.
        """
        dest = goto_dest(inst)
        if is_label_exp(dest):
            insts = labels.loopup_label(label_exp_label(dest))
            return lambda: self._pc.set_contents(insts)
        elif is_register_exp(dest):
            reg = self.get_register(register_exp_reg(dest))
            return lambda: self._pc.set_contents(reg.get_contents())
        else:
            raise Exception(f"Bad goto instruction: {inst}")

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
def is_tagged_list(pair: List, tag: str):
    assert isinstance(pair, list), pair
    assert isinstance(pair[0], str), pair
    return pair[0] == tag

def is_operation_exp(exp: List):
    """Is the expression an operation expression or primitive expression?
    e.g.
     [['op', '-'], ['reg', 'counter'], ['const', 1]]
    """
    assert isinstance(exp, list)
    return is_tagged_list(exp[0], 'op')

def operation_exp_op(exp: List) -> str:
    """Operator of the operation expression."""
    return exp[0][1]

def operation_exp_operands(exp: List) -> List:
    """Operands of the operation expression."""
    return exp[1:]

def test_condition(inst) -> List:
    """
    [(op =) (reg counter) (const 0)]
    """
    return inst[1:]

def branch_dest(inst) -> List:
    """
    [branch (label expt-done)]
    """
    assert len(inst) == 2
    return inst[1]

def assign_reg_name(inst) -> str:
    """The register name of assignment instruction."""
    return inst[1]

def assign_value_exp(inst) -> List:
    """The value expression of assignment instrcution."""
    return inst[2:]

def goto_dest(exp: List) -> List:
    return exp[1]

def make_operation_exp(exp: List, machine: Machine, labels: LabelTable) -> Callable:
    """Make the procedure from operation expression.
    Steps:
    1. Extract operator from the expression, and lookup primitive operation from the
    machine operations table.
    2. Extract operands from the expression, and make a list of primitive expression.
    3. Return a procedure that apply the primitive expression values to the operation.

    e.g.
    operation exp: [['op', '*'], ['reg', 'b'], ['reg', 'product']]
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
constant_exp_value = register_exp_reg = label_exp_label = lambda exp: exp[1]

def make_primitive_exp(exp: List, machine: Machine, labels: LabelTable) -> Callable:
    """Return a procedure that returns corresponding value of the expression.
    e.g. a const value, instruction list the label points to, register contents.
    A primitive expression contains only *const*, *label*, *reg*.
    e.g.
    ['const', 1], ['label', 'expt-loop'], ['reg', 'counter']
    """
    assert isinstance(exp, list)
    assert len(exp) == 2

    if is_constant_exp(exp):
        return lambda: constant_exp_value(exp)
    elif is_label_exp(exp):
        label = label_exp_label(exp)
        insts = labels.lookup_label(label)
        return lambda: insts
    elif is_register_exp(exp):
        reg = register_exp_reg(exp)
        r = machine.get_register(reg)
        return lambda: r.get_contents()
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
