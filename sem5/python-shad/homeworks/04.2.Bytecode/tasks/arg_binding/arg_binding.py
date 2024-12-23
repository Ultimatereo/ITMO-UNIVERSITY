from types import FunctionType
from typing import Any

CO_VARARGS = 4
CO_VARKEYWORDS = 8

ERR_TOO_MANY_POS_ARGS = 'Too many positional arguments'
ERR_TOO_MANY_KW_ARGS = 'Too many keyword arguments'
ERR_MULT_VALUES_FOR_ARG = 'Multiple values for arguments'
ERR_MISSING_POS_ARGS = 'Missing positional arguments'
ERR_MISSING_KWONLY_ARGS = 'Missing keyword-only arguments'
ERR_POSONLY_PASSED_AS_KW = 'Positional-only argument passed as keyword argument'


def has_varargs(func):  # type: ignore
    if isinstance(func, FunctionType):
        code_flags = func.__code__.co_flags
        return (code_flags & CO_VARARGS) != 0
    return False


def has_kwarg(func):  # type: ignore
    if isinstance(func, FunctionType):
        code_flags = func.__code__.co_flags
        return (code_flags & CO_VARKEYWORDS) != 0
    return False


def bind_args(func: FunctionType, *args: Any, **kwargs: Any) -> dict[str, Any]:
    """Bind values from `args` and `kwargs` to corresponding arguments of `func`

    :param func: function to be inspected
    :param args: positional arguments to be bound
    :param kwargs: keyword arguments to be bound
    :return: `dict[argument_name] = argument_value` if binding was successful,
             raise TypeError with one of `ERR_*` error descriptions otherwise
    """
    _arg_names = list(func.__code__.co_varnames)
    arg_defaults = func.__defaults__ or ()
    kw_defaults = func.__kwdefaults__ or {}
    _posonlyargcount = func.__code__.co_posonlyargcount
    _kwonlyargcount = func.__code__.co_kwonlyargcount
    _arg_count = func.__code__.co_argcount + _kwonlyargcount
    posonly_args = set()
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
        elif i >= func.__code__.co_argcount:
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
    bound_args: dict[str, Any] = {}
    if has_vararg:
        bound_args[vararg_name] = ()
    if has_kwararg:
        bound_args[kwararg_name] = {}

    if not has_vararg and len(arg_values) > _arg_count - _kwonlyargcount:
        raise TypeError(ERR_TOO_MANY_POS_ARGS)

    # Bind positional arguments
    for i in range(len(arg_values)):
        if arg_values[i] == kwararg_name:
            continue
        if i < func.__code__.co_argcount:
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
        arg_name = _arg_names[func.__code__.co_argcount - len(arg_defaults) + i]
        arg_default = arg_defaults[i]
        if arg_name not in bound_args:
            bound_args[arg_name] = arg_default

    for key, value in kw_defaults.items():
        if key not in bound_args:
            bound_args[key] = value

    # Checking for missing positional arguments
    for arg_name in posonly_args:
        if arg_name not in bound_args:
            raise TypeError(ERR_MISSING_POS_ARGS)
    # Checking for missing positional arguments
    for arg_name in usual_args:
        if arg_name not in bound_args:
            raise TypeError(ERR_MISSING_POS_ARGS)

    # Checking for missing keyword-only arguments
    for kwarg_name in kwonly_args:
        if kwarg_name not in bound_args:
            raise TypeError(ERR_MISSING_KWONLY_ARGS)

    return bound_args
