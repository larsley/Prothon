# Copyright (c) 2022-2023 by Lars Leyendecker
#
# This program and the accompanying materials are made available under
# the terms of the Eclipse Public License v2.0 which accompanies this
# distribution and is available at:
#
#     http://www.eclipse.org/legal/epl-2.0/
#
# This program may also be made available under the following secondary
# licenses when the conditions for such availability set forth in the
# Eclipse Public License v2.0 are satisfied:
#
#    GNU General Public License, Version 2.0, or any later versions of
#    that license
#
# SPDX-License-Identifier: EPL-2.0 OR GPL-2.0-or-later

import builtins
import numpy as np
from typing import Any, Optional, Callable


class Parser:
    def __init__(self, objects: dict, mods: dict):
        self.objects = objects
        self.mods = mods

    def parse_directly(self, code: str) -> Any:
        """Parses code to an object

        The code may use py[ID] to access the self.objects dictionary.

        :param str code: code to be evaluated
        :return Any: parsed object
        """
        py = self.objects
        return eval(compile(code, filename="<string>", mode="eval"))

    def parse_parameters(self, arg_string: str) -> tuple[list, dict]:
        """Parses function parameters

        :param str arg_string: Argument-string to parse
        :return tuple[list, dict]: (Args, Keyword Args)
        """
        return self.parse_directly(arg_string)

    def parse_call(
        self, call_str: str, whitelist: Optional[list[str]] = None
    ) -> tuple[Callable, tuple[list, dict]]:
        """Parses a call

        :param str call_str: String of the call
        :param Optional[list[str]] whitelist: Whitelist of built-ins, optional
        :return tuple[str, tuple[list, dict]]: (Function, (Args, Keyword Args))
        """
        function_str, parameter_str = call_str.split(",", 1)
        function = self.parse_function(function_str, whitelist)
        parameters = self.parse_parameters(parameter_str)
        return function, parameters

    def obj_to_str(self, obj: Any, deep: Optional[bool] = True) -> str:
        """Turns an object into a string representation

        If object is a collection and deep is True, the elements of the
        collection will also be turned into a string representation
        recursively.

        :param Any obj:
        :param Optional[bool] deep: Whether the object should be derefecenced
                                    deep or shallow
        :return:
        """
        # if deep, the elements get handled recursively
        # if not, the elements are just added to the reference dictionary
        if deep:
            element_handler = self.obj_to_str
        else:
            element_handler = self.add_object

        if obj is None or isinstance(obj, (int, float, bool)):
            return str(obj)
        elif isinstance(obj, str):
            return f'"{escape_string(obj)}"'
        elif isinstance(obj, np.ndarray):
            if deep:
                return self.obj_to_str(obj.tolist(), deep=True)
            else:
                return self.obj_to_str(list(obj), deep=False)
        elif isinstance(obj, np.generic):
            return self.obj_to_str(obj.tolist(), deep=deep)
        elif isinstance(obj, list):
            return f"[{','.join([element_handler(o) for o in obj])}]"
        elif isinstance(obj, tuple):
            return f"({','.join([element_handler(o) for o in obj])})"
        elif isinstance(obj, dict):
            elements = [
                f"{self.obj_to_str(key)}:{element_handler(o)}"
                for key, o in obj.items()
            ]
            return f"{{{','.join(elements)}}}"

        # everything that was not handled before is regarded as an
        # unknown object and added to the reference dictionary
        else:
            self.add_object(obj)

    def add_object(self, obj: object) -> str:
        """Adds an object to the reference dictionary

        The object gets added to the reference dictionary using
        its python object id as the key.
        If the object was already in the dictionary, nothing happens.
        It is impossible for another object to share the same id, while both
        objects are in memory. Therefore, an object can never be overwritten
        by a different one.

        :param object obj:
        :return str: String representation of the reference "py[ID]"
        """
        self.objects[id(obj)] = obj
        return f"py[{str(id(obj))}]"

    def parse_function(
        self, function: str, whitelist: Optional[list[str]] = None
    ):
        """

        If the function is a built-in, it is checked against the whitelist
        and a FunctionNotWhitelisted might be raised.
        If it's not a built-in, the module its in is searched and the function
        returned.
        If no module or no function can be found, an exception is raised.

        :param function:
        :param Optional[list[str]] whitelist: Whitelist of built-ins, optional
        :return:
        """
        parts = function.split(".")
        if len(parts) == 1:
            if whitelist is not None and function not in whitelist:
                raise FunctionNotWhitelisted(function)
            return getattr(builtins, function)
        else:
            module = ".".join(parts[:-1])
            func = parts[-1]
            return getattr(self.mods[module], func)


def escape_string(s: str) -> str:
    """Escapes a string

    :param str s: string to escape
    :return str: escaped string
    """
    return s.replace("\\", "\\\\").replace('"', '\\"')


class FunctionNotWhitelisted(Exception):
    def __init__(self, function):
        msg = f"The buildin function '{function}' is not whitelisted!"
        super().__init__(self, msg)
