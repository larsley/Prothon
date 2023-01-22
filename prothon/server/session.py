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

from parser import Parser
from typing import Any
import importlib
from argparse import Namespace
from fnmatch import fnmatch

FAILURE = 1
SUCCESS = 0


def extract_exception_data(e: Exception) -> tuple[str, str]:
    """Extracts the name and string representation of an exception

    :param Exception e: Exception
    :return tuple[str, str]: (name, string representation) of the exception
    """
    return e.__class__.__name__, str(e)


class Session:
    def __init__(self, args: Namespace):
        """init Session

        :param Namespace args: Command line arguments
        """
        self.args = args
        self.objects = dict()
        self.mods = dict()
        self.parser = Parser(self.objects, self.mods)
        self.answers = list()

    def delete(self, keys: list[int]) -> None:
        """Deletes keys from the reference dictionary

        :param [int] keys: list of keys to be deleted
        :return: None
        """

        for key in keys:
            if key in self.objects:
                del self.objects[key]

    def keep(self, keys: list[int]) -> None:
        """Deletes all keys from the reference dict except those specified

        :param list[int] keys: list of keys to be kept
        :return None: None
        """

        for key in list(self.objects.keys()):
            if key not in keys:
                del self.objects[key]

    def generate_answer(self, data: str) -> str:
        """Formats the answer on the top of the stack

        The top of the answer stack is pushed and the answer gets formatted
        according to the depth.

        :param str data: Dereferentiation depth
        :return str: Formatted answer
        """

        depth = int(data)
        try:
            answer = self.answers.pop()
        except IndexError:
            raise AnswerStackEmptyException("No answer to serve!")

        return self.format_answer(*answer, depth)

    def format_answer(self, status: int, obj: object, depth: int) -> str:
        """Formats the answer

        Formats the answer based on the status and required
        dereferentiation depth.
        FAILUREs always get dereferenced completely.

        :param int status: SUCCESS or FAILURE of the answer's query
        :param object obj: Object the query returned
        :param int depth: Dereferentiation depth
        :return str: Formatted answer
        """
        if obj is None or status == FAILURE or depth == -1:
            answer = self.parser.obj_to_str(obj, deep=True)
        elif depth == 1:
            answer = self.parser.obj_to_str(obj, deep=False)
        else:
            answer = self.parser.add_object(obj)
        return f"{status}{answer}"

    def handle_query(self, query: str) -> str:
        """Parses and executes a query

        If an Exception is raised during parsing of the query or if
        an answer is requested, an answer will be returned immediately.

        Otherwise, the query is executed and the answer pushed to the stack.
        In this case an empty string is returned as an answer.

        :param str query:
        :return str: Answer to send to the client
        """
        try:
            command, data = split_query(query)
            if command == "fetch":
                return self.generate_answer(data)
        except Exception as e:
            obj = extract_exception_data(e)
            return self.format_answer(FAILURE, obj, depth=-1)

        try:
            obj = self.execute_query(command, data)
        except Exception as e:
            obj = extract_exception_data(e)
            status = FAILURE
        else:
            status = SUCCESS
        self.answers.append((status, obj))
        return ""

    def execute_query(self, command: str, data: str) -> Any:
        """Executes a query

        :param str command: Command to execute
        :param str data: Data for the command
        :return Any: Return value of the command
        """
        if command == "call":
            return self.execute_call(data)
        elif command == "del":
            return self.execute_delete(data)
        elif command == "keep":
            return self.execute_keep(data)
        elif command == "import":
            return self.execute_import(data)
        elif command == "method":
            return self.execute_method(data)
        elif command == "push":
            return self.execute_push(data)
        else:
            raise UnrecognizedCommandException(command)

    def execute_call(self, query: str) -> Any:
        """Parses and executes a function call

        :param str query: call-query to parse
        :return Any: return value of the call
        """
        function, (args, keyword_args) = self.parser.parse_call(
            query, self.args.buildin
        )
        return function(*args, **keyword_args)

    def execute_delete(self, keys_str: str) -> None:
        """Parses and executes a call to the garbage collector

        :param str keys_str: Keys to delete (comma separated ints)
        :return None: None
        """
        keys = self.parser.parse_directly(keys_str)
        self.delete(keys)

    def execute_import(self, query: str) -> None:
        """Parses an import query and imports the module

        The module gets imported using its name and inserted
        into the mods dictionary with its alias as the key.

        Raises ModuleNotWhiteListed if the module is not whitelisted.

        :param str query: Import query "Name,Alias"
        :return None: None
        """
        module_name, module_alias = query.split(",")
        if not self.module_is_whitelisted(module_name):
            raise ModuleNotWhitelisted
        self.mods[module_alias] = importlib.import_module(module_name)
        return None

    def module_is_whitelisted(self, module: str) -> bool:
        """Is the module whitelisted?

        Tries to match the module against any of the patterns
        in the whitelist.

        :param str module: Module in question
        :return bool: Whether the module is whitelisted
        """
        if self.args.module is None:
            return True
        else:
            for pattern in self.args.module:
                if fnmatch(module, pattern):
                    return True
        return False

    def execute_method(self, query: str) -> Any:
        """Parses and executes a method

        :param str query: Query to execute "Object,Method,Parameters"
        :return Any: Return value of the method
        """
        obj_str, method_str, parameters_str = query.split(",", 2)
        obj = self.parser.parse_parameters(obj_str)
        args, keyword_args = self.parser.parse_parameters(parameters_str)
        method = getattr(obj, method_str)
        return method(*args, **keyword_args)

    def execute_keep(self, keys_str) -> None:
        """Deletes every reference except the ones specified

        :param str keys_str: Keys to keep (comma separated ints)
        :return None: None
        """
        keys = self.parser.parse_directly(keys_str)
        self.keep(keys)

    def execute_push(self, data) -> object:
        """Returns the referenced object

        :param str data: Id of an object
        :return object: The referenced object
        """
        return self.objects[self.parser.parse_directly(data)]


def split_query(query: str) -> tuple[str, str]:
    """Splits a query into command and data

    Raises MalformedQueryException if the query can not
    be split.

    :param str query: query "command,data"
    :return tuple[str, str]: ("command", "data")
    """
    try:
        command, data = query.split(",", 1)
    except ValueError as e:
        raise MalformedQueryException(
            "Unable to extract answer mode and command!"
        )
    return command, data


class ModuleNotWhitelisted(Exception):
    pass


class MalformedQueryException(Exception):
    pass


class AnswerStackEmptyException(Exception):
    pass


class UnrecognizedCommandException(Exception):
    def __init__(self, command):
        msg = f"The command '{command}' is not recognized!"
        super().__init__(self, msg)
