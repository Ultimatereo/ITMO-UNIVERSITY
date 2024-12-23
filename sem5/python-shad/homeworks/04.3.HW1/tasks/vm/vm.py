"""
Simplified VM code which works for some cases.
You need extend/rewrite code to pass all cases.
"""

import builtins
import dis
import types
import typing as tp

CO_VARARGS = 4
CO_VARKEYWORDS = 8

ERR_TOO_MANY_POS_ARGS = 'Too many positional arguments'
ERR_TOO_MANY_KW_ARGS = 'Too many keyword arguments'
ERR_MULT_VALUES_FOR_ARG = 'Multiple values for arguments'
ERR_MISSING_POS_ARGS = 'Missing positional arguments'
ERR_MISSING_KWONLY_ARGS = 'Missing keyword-only arguments'
ERR_POSONLY_PASSED_AS_KW = 'Positional-only argument passed as keyword argument'

CMP_OP = dis.cmp_op


def has_varargs(func):  # type: ignore
    if isinstance(func, types.CodeType):
        code_flags = func.co_flags
        return (code_flags & CO_VARARGS) != 0
    return False


def has_kwarg(func):  # type: ignore
    if isinstance(func, types.CodeType):
        code_flags = func.co_flags
        return (code_flags & CO_VARKEYWORDS) != 0
    return False


def bind_args(func: types.CodeType, arg_defaults: tuple[tp.Any, ...],
              kw_defaults: dict[str, tp.Any], *args: tp.Any, **kwargs: tp.Any) -> dict[str, tp.Any]:
    """Bind values from `args` and `kwargs` to corresponding arguments of `func`

    :param func: function to be inspected
    :param arg_defaults: tuple of default pos arguments
    :param kw_defaults: dict of default kw arguments
    :param args: positional arguments to be bound
    :param kwargs: keyword arguments to be bound
    :return: `dict[argument_name] = argument_value` if binding was successful,
             raise TypeError with one of `ERR_*` error descriptions otherwise
    """
    _arg_names = list(func.co_varnames)
    # arg_defaults = func.__defaults__ or ()
    # kw_defaults = func.__kwdefaults__ or {}
    _posonlyargcount = func.co_posonlyargcount
    _kwonlyargcount = func.co_kwonlyargcount
    _arg_count = func.co_argcount + _kwonlyargcount
    posonly_args = set([])
    usual_args = set()
    kwonly_args = set()
    has_vararg = has_varargs(func)
    if has_vararg:
        _arg_count += 1
    has_kwararg = has_kwarg(func)
    if has_kwararg:
        _arg_count += 1
    vararg_name = ''
    kwararg_name = ''
    num = _arg_count
    if has_kwararg:
        num -= 1
    if has_vararg:
        num -= 1
    for i in range(num):
        if i < _posonlyargcount:
            posonly_args.add(_arg_names[i])
        elif i >= func.co_argcount:
            kwonly_args.add(_arg_names[i])
        else:
            usual_args.add(_arg_names[i])
    if has_kwararg:
        kwararg_name = _arg_names[_arg_count - 1]
    if has_vararg:
        if has_kwararg:
            vararg_name = _arg_names[_arg_count - 2]
        else:
            vararg_name = _arg_names[_arg_count - 1]

    arg_values = list(args)
    bound_args: dict[str, tp.Any] = {}
    if has_vararg:
        bound_args[vararg_name] = ()
    if has_kwararg:
        bound_args[kwararg_name] = {}

    if not has_vararg and len(arg_values) > _arg_count - _kwonlyargcount:
        raise TypeError(ERR_TOO_MANY_POS_ARGS)

    # Bind positional arguments
    for i in range(len(arg_values)):
        if i < func.co_argcount:
            if _arg_names[i] in kwonly_args:
                raise TypeError(ERR_POSONLY_PASSED_AS_KW)
            else:
                bound_args[_arg_names[i]] = arg_values[i]
        elif has_vararg:
            bound_args[vararg_name] += (arg_values[i],)
        else:
            raise TypeError(ERR_TOO_MANY_POS_ARGS)

    # Bind keyword arguments
    for key, value in kwargs.items():
        if key in usual_args or key in kwonly_args:
            if key in bound_args:
                raise TypeError(ERR_MULT_VALUES_FOR_ARG)
            bound_args[key] = value
        elif has_kwararg:
            bound_args[kwararg_name][key] = value
        elif key in posonly_args:
            raise TypeError(ERR_POSONLY_PASSED_AS_KW)
        else:
            raise TypeError(ERR_TOO_MANY_KW_ARGS)

    # Set default values for missing arguments
    for i in range(len(arg_defaults)):
        arg_name = _arg_names[func.co_argcount - len(arg_defaults) + i]
        arg_default = arg_defaults[i]
        if arg_name not in bound_args:
            bound_args[arg_name] = arg_default

    for key, value in kw_defaults.items():
        if key not in bound_args:
            bound_args[key] = value

    # Check for missing positional arguments
    for arg_name in posonly_args:
        if arg_name not in bound_args:
            raise TypeError(ERR_MISSING_POS_ARGS)
    # Check for missing positional arguments
    for arg_name in usual_args:
        if arg_name not in bound_args:
            raise TypeError(ERR_MISSING_POS_ARGS)

    # Check for missing keyword-only arguments
    for kwarg_name in kwonly_args:
        if kwarg_name not in bound_args:
            raise TypeError(ERR_MISSING_KWONLY_ARGS)

    return bound_args


class Frame:
    """
    Frame header in cpython with description
        https://github.com/python/cpython/blob/3.11/Include/frameobject.h

    Text description of frame parameters
        https://docs.python.org/3/library/inspect.html?highlight=frame#types-and-members
    """

    def __init__(self,
                 frame_code: types.CodeType,
                 frame_builtins: dict[str, tp.Any],
                 frame_globals: dict[str, tp.Any],
                 frame_locals: dict[str, tp.Any]) -> None:
        self.has_return = False
        self.code = frame_code
        self.builtins = frame_builtins
        self.globals = frame_globals
        self.locals = frame_locals
        self.data_stack: tp.Any = []
        self.return_value = None
        self.instructions = list(iter(dis.get_instructions(self.code)))
        self.offset_to_index = {el.offset: i for i, el in enumerate(self.instructions)}
        self.curIndex = 0
        self.tupleForFunction: tuple[str, ...] = tuple()

    def top(self) -> tp.Any:
        return self.data_stack[-1]

    def pop(self) -> tp.Any:
        return self.data_stack.pop()

    def push(self, *values: tp.Any) -> None:
        self.data_stack.extend(values)

    def popn(self, n: int) -> tp.Any:
        """
        Pop a number of values from the value stack.
        A list of n values is returned, the deepest value first.
        """
        if n > 0:
            returned = self.data_stack[-n:]
            self.data_stack[-n:] = []
            return returned
        else:
            return []

    def run(self) -> tp.Any:
        while self.curIndex != len(self.instructions) and not self.has_return:
            instruction = self.instructions[self.curIndex]
            instruction_name = instruction.opname.lower() + "_op"
            if instruction_name == "kw_names_op" or instruction_name == "load_global_op":
                getattr(self, instruction_name)(instruction.arg)
            else:
                getattr(self, instruction_name)(instruction.argval)
            self.curIndex += 1
        return self.return_value

    def copy_op(self, i: int) -> None:
        obj: list[tp.Any] = self.popn(i)
        obj.insert(0, obj[-1])
        for i in range(len(obj) - 1, -1, -1):
            self.push(obj[i])

    def load_assertion_error_op(self, arg: tp.Any) -> None:
        self.push(AssertionError())

    def raise_varargs_op(self, argc: int) -> None:
        if argc == 0:
            raise
        elif argc == 1:
            raise self.get_tos()
        elif argc == 2:
            tos = self.get_tos()
            tos1 = self.get_tos()
            raise tos1 from tos
        else:
            assert False, "Unknown type of raise varargs: " + str(argc)

    def for_iter_op(self, delta: int) -> None:
        iterator: tp.Any = self.get_tos()
        try:
            n = iterator.__next__()
            self.push(iterator)
            self.push(n)
        except StopIteration:
            self.curIndex = self.offset_to_index[delta] - 1

    def get_iter_op(self, arg: tp.Any) -> None:
        self.push(iter(self.pop()))

    def contains_op_op(self, invert: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if invert == 1:
            self.push(tos1 not in tos)
        else:
            self.push(tos1 in tos)

    def is_op_op(self, invert: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if invert == 1:
            self.push(tos1 is not tos)
        else:
            self.push(tos1 is tos)

    def store_global_op(self, arg: str) -> None:
        const = self.pop()
        self.globals[arg] = const

    def delete_global(self, name: str) -> None:
        self.globals.pop(name)

    def resume_op(self, arg: int) -> None:
        pass

    def push_null_op(self, arg: int) -> None:
        self.push(None)

    def precall_op(self, arg: int) -> None:
        pass

    def call_op(self, arg: int) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-CALL
        """
        args = self.popn(arg)
        _self = self.pop()
        if len(self.data_stack) != 0:
            self.pop()
        pos_args = args[:arg - len(self.tupleForFunction)]
        kw_args = {}
        for i, key in enumerate(self.tupleForFunction):
            kw_args[key] = args[arg - len(self.tupleForFunction) + i]
        self.push(_self(*pos_args, **kw_args))
        self.tupleForFunction = tuple('')

    def call_function_ex_op(self, flags: int) -> None:
        kwargs = {}
        if flags % 2 == 1:
            kwargs = self.pop()
        args = self.pop()
        _callable = self.pop()
        self.push(_callable(*args, **kwargs))

    def load_name_op(self, arg: str) -> None:
        """
        Partial realization

        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_NAME
        """
        if arg in self.locals:
            self.push(self.locals[arg])
        elif arg in self.globals:
            self.push(self.globals[arg])
        elif arg in self.builtins:
            self.push(self.builtins[arg])
        else:
            raise NameError

    def swap_op(self, arg: int) -> None:
        obj = self.popn(arg)
        obj[arg - 1], obj[0] = obj[0], obj[arg - 1]
        for i in range(len(obj) - 1, -1, -1):
            self.push(obj[i])

    def delete_name_op(self, name: str) -> None:
        self.locals.pop(name)

    def store_fast_op(self, var_num: str) -> None:
        self.locals[var_num] = self.pop()

    def load_fast_op(self, var_num: str) -> None:
        if var_num.startswith("."):
            self.push(self.locals['args'][int(var_num[1:])])
        else:
            if var_num not in self.locals and var_num not in self.globals and var_num not in self.builtins:
                raise UnboundLocalError
            self.push(self.locals[var_num])

    def load_fast_check_op(self, var_num: str) -> None:
        if var_num not in self.locals and var_num not in self.globals and var_num not in self.builtins:
            raise UnboundLocalError
        self.push(self.locals[var_num])

    def load_fast_and_clear_op(self, var_num: str) -> None:
        if var_num not in self.locals and var_num not in self.globals and var_num not in self.builtins:
            self.locals[var_num] = None
        self.push(self.locals[var_num])

    def load_global_op(self, argi: int) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_GLOBAL
        """
        arg = self.code.co_names[argi >> 1]
        if argi % 2 == 1:
            self.push(None)
        if arg in self.globals:
            self.push(self.globals[arg])
        elif arg in self.builtins:
            self.push(self.builtins[arg])
        else:
            raise NameError

    def load_const_op(self, arg: tp.Any) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-LOAD_CONST
        """
        self.push(arg)

    def get_tos(self) -> tp.Any:
        tos = None
        while True and len(self.data_stack) > 0:
            tos = self.pop()
            if tos is not None:
                break
        return tos

    def unpack_sequence_op(self, arg: tp.Any) -> None:
        package = self.get_tos()
        for i in range(len(package) - 1, -1, -1):
            self.push(package[i])

    def build_slice_op(self, argc: int) -> None:
        if argc == 2:
            tos = self.pop()
            tos1 = self.pop()
            self.push(slice(tos1, tos))
        elif argc == 3:
            tos = self.pop()
            tos1 = self.pop()
            tos2 = self.pop()
            self.push(slice(tos2, tos1, tos))
        else:
            assert False, "Not a right number for build_slice: " + str(argc)

    def format_value_op(self, flags: tuple[tp.Any, ...]) -> None:
        value = self.get_tos()
        fmt_spec = ""

        t: type | None = None
        has_fmt: bool = False
        if len(flags) == 1:
            t = flags[0]
        elif len(flags) > 1:
            t = flags[0]
            has_fmt = flags[1]
        if has_fmt:
            fmt_spec = self.get_tos()
            fmt_spec, value = value, fmt_spec
        if t is not None:
            value = t(value)

        if has_fmt:
            self.push(value.format(fmt_spec))
        else:
            self.push(value)

    def import_name_op(self, name: str) -> None:
        tos = self.pop()
        tos1 = self.pop()
        self.push(__import__(name, level=tos1, fromlist=tos, globals=self.globals, locals=self.locals))

    def import_from_op(self, name: str) -> None:
        tos = self.get_tos()
        self.push(getattr(tos, name))

    def import_star_op(self, arg: tp.Any) -> None:
        tos = self.get_tos()
        for func in dir(tos):
            if not func.startswith("_"):
                self.locals[func] = getattr(tos, func)

    def load_attr_op(self, name: str) -> None:
        tos = self.get_tos()
        self.push(getattr(tos, name))

    def store_attr_op(self, name: str) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        setattr(tos, name, tos1)
        self.push(tos1)

    def delete_attr_op(self, name: str) -> None:
        tos = self.get_tos()
        delattr(tos, name)
        self.push(tos)

    def delete_fast_op(self, var: str) -> None:
        self.locals.pop(var)

    def build_list_op(self, count: int) -> None:
        self.push(list(self.popn(count)))

    def build_set_op(self, count: int) -> None:
        self.push(set(self.popn(count)))

    def build_tuple_op(self, count: int) -> None:
        self.push(tuple(self.popn(count)))

    def build_map_op(self, count: int) -> None:
        entries = self.popn(2 * count)
        d = {}
        for i in range(0, len(entries), 2):
            d[entries[i]] = entries[i + 1]
        self.push(d)

    def load_method_op(self, name: str) -> None:
        tos = self.get_tos()
        if hasattr(tos, name):
            self.push(tos)
            self.push(getattr(tos, name))
        else:
            self.push(None)
            self.push(getattr(tos, name))

    def build_const_key_map_op(self, count: int) -> None:
        keys = self.get_tos()
        values = self.popn(count)
        d = {}
        for i in range(count):
            d[keys[i]] = values[i]
        self.push(d)

    def build_string_op(self, count: int) -> None:
        ans = ""
        for i in range(count):
            ans = str(self.get_tos()) + ans
        self.push(ans)

    def list_to_tuple_op(self, arg: tp.Any) -> None:
        self.push(tuple(self.get_tos()))

    def list_extend_op(self, i: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if len(tos1) == 0:
            self.push(list(tos))
        else:
            # list.extend(tos1[-i], tos)
            for ii in range(len(tos)):
                tos1.append(tos[ii])
            self.push(tos1)

    def set_update_op(self, i: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if len(tos1) == 0:
            self.push(set(tos))
        else:
            for ii in range(i):
                tos1.update(tos)
            self.push(tos1)

    def dict_update_op(self, i: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if len(tos1) == 0:
            self.push(dict(tos))
        else:
            for k, v in tos.items():
                tos1[k] = v
            self.push(tos1)

    def unary_positive_op(self, arg: tp.Any) -> None:
        self.push(+self.get_tos())

    def unary_negative_op(self, arg: tp.Any) -> None:
        self.push(-self.get_tos())

    def unary_not_op(self, arg: tp.Any) -> None:
        self.push(not self.get_tos())

    def unary_invert_op(self, arg: tp.Any) -> None:
        self.push(~self.get_tos())

    def unary_get_iter_op(self, arg: tp.Any) -> None:
        self.push(iter(self.get_tos()))

    def get_yield_from_iter_op(self, arg: tp.Any) -> None:
        tos = self.get_tos()
        # TODO: not everything should be iter!
        self.push(iter(tos))

    def yield_value_op(self, arg: tp.Any) -> None:
        self.return_value = self.pop()

    def binary_op_op(self, op: int) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        if op == 0 or op == 13:
            self.push(tos1 + tos)
        elif op == 1 or op == 14:
            self.push(tos1 & tos)
        elif op == 2 or op == 15:
            self.push(tos1 // tos)
        elif op == 3 or op == 16:
            self.push(tos1 << tos)
        elif op == 5 or op == 18:
            self.push(tos1 * tos)
        elif op == 6 or op == 19:
            self.push(tos1 % tos)
        elif op == 7 or op == 20:
            self.push(tos1 | tos)
        elif op == 8 or op == 21:
            self.push(tos1 ** tos)
        elif op == 9 or op == 22:
            self.push(tos1 >> tos)
        elif op == 10 or op == 23:
            self.push(tos1 - tos)
        elif op == 11 or op == 24:
            self.push(tos1 / tos)
        elif op == 12 or op == 25:
            self.push(tos1 ^ tos)

        else:
            assert False, "Unknown op: " + str(op)

    def binary_subscr_op(self, arg: tp.Any) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        self.push(tos1[tos])

    def store_subscr_op(self, arg: tp.Any) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        tos2 = self.get_tos()
        tos1[tos] = tos2
        self.push(tos2, tos1, tos)

    def delete_subscr_op(self, arg: tp.Any) -> None:
        tos = self.get_tos()
        tos1 = self.get_tos()
        del tos1[tos]
        self.push(tos1, tos)

    def compare_op_op(self, op: str) -> None:
        while True:
            tos = self.pop()
            if tos is not None:
                break
        while True:
            tos1 = self.pop()
            if tos1 is not None:
                break
        if op == "<":
            self.push(tos1 < tos)
        elif op == "<=":
            self.push(tos1 <= tos)
        elif op == "==":
            self.push(tos1 == tos)
        elif op == "!=":
            self.push(tos1 != tos)
        elif op == ">":
            self.push(tos1 > tos)
        elif op == ">=":
            self.push(tos1 >= tos)
        else:
            assert False, "Unknown compare op: " + op

    def pop_jump_forward_if_none_op(self, delta: int) -> None:
        tos = self.pop()
        if tos is None:
            self.curIndex = self.offset_to_index[delta] - 1

    def pop_jump_backward_if_none_op(self, delta: int) -> None:
        tos = self.pop()
        if tos is None:
            self.curIndex = self.offset_to_index[delta] - 1

    def pop_jump_forward_if_not_none_op(self, delta: int) -> None:
        tos = self.pop()
        if tos is not None:
            self.curIndex = self.offset_to_index[delta] - 1

    def pop_jump_backward_if_not_none_op(self, delta: int) -> None:
        tos = self.pop()
        if tos is not None:
            self.curIndex = self.offset_to_index[delta] - 1

    def jump_if_true_or_pop_op(self, delta: int) -> None:
        if a := self.get_tos():
            self.curIndex = self.offset_to_index[delta] - 1
            self.push(a)

    def jump_if_false_or_pop_op(self, delta: int) -> None:
        if not (a := self.get_tos()):
            self.curIndex = self.offset_to_index[delta] - 1
            self.push(a)

    def jump_backward_op(self, delta: int) -> None:
        self.curIndex = self.offset_to_index[delta] - 1

    def jump_backward_no_interrupt_op(self, delta: int) -> None:
        self.curIndex = self.offset_to_index[delta] - 1

    def jump_backward_if_true_op(self, delta: int) -> None:
        if self.get_tos():
            self.curIndex = self.offset_to_index[delta] - 1

    def jump_backward_if_false_op(self, delta: int) -> None:
        if not self.get_tos():
            self.curIndex = self.offset_to_index[delta] - 1

    def pop_jump_forward_if_true_op(self, delta: int) -> None:
        # while True:
        #     tos = self.pop()
        #     if tos is not None:
        #         break
        if self.get_tos():
            self.curIndex = self.offset_to_index[delta] - 1

    def pop_jump_forward_if_false_op(self, delta: int) -> None:
        if not self.get_tos():
            self.curIndex = self.offset_to_index[delta] - 1

    def jump_forward_op(self, delta: int) -> None:
        self.curIndex = self.offset_to_index[delta] - 1

    def nop_op(self, arg: tp.Any) -> None:
        pass

    def return_generator_op(self, arg: tp.Any) -> None:
        pass

    def return_value_op(self, arg: tp.Any) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-RETURN_VALUE
        """
        self.return_value = self.pop()
        self.has_return = True

    def pop_top_op(self, arg: tp.Any) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-POP_TOP
        """
        if len(self.data_stack) > 0:
            self.pop()

    def kw_names_op(self, arg: int) -> None:
        self.tupleForFunction = self.code.co_consts[arg]

    def make_function_op(self, arg: int) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-MAKE_FUNCTION
        """
        code = self.pop()  # the code associated with the function (at TOS1)
        tuple_defaults: tuple[tp.Any, ...] = tuple()
        dict_defaults = {}
        if arg % 2 == 1:
            tuple_defaults = self.pop()
        elif (arg // 2) % 2 == 1:
            dict_defaults = self.pop()
        # elif (arg // 4) % 2 == 1:
        #     tuple_strings = self.pop()
        # elif (arg // 4) % 2 == 1:
        #     tuple_cells = self.pop()

        def f(*args: tp.Any, **kwargs: tp.Any) -> tp.Any:
            parsed_args = bind_args(code, tuple_defaults, dict_defaults, *args, **kwargs)
            f_locals = dict(self.locals)
            f_locals.update(parsed_args)

            frame = Frame(code, self.builtins, self.globals, f_locals)  # Run code in prepared environment
            return frame.run()

        self.push(f)

    def store_name_op(self, arg: str) -> None:
        """
        Operation description:
            https://docs.python.org/release/3.11.5/library/dis.html#opcode-STORE_NAME
        """
        const = self.pop()
        self.locals[arg] = const


class VirtualMachine:
    def run(self, code_obj: types.CodeType) -> None:
        """
        :param code_obj: code for interpreting
        """
        globals_context: dict[str, tp.Any] = {}
        frame = Frame(code_obj, builtins.globals()['__builtins__'], globals_context, globals_context)
        return frame.run()
