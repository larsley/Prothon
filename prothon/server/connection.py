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

import socket
import logging

import session
from argparse import Namespace
from typing import Generator

ENCODING = "latin-1"
BYTES_DIGITS = 10


def connection_loop(connection: socket.socket, args: Namespace) -> None:
    """Main loop of a single connection

    Runs as long as the socket connection is active. It is broken
    when the connection is closed.
    In every loop one query is executed and possibly one answer is send.

    :param socket connection:
    :param Namespace args:
    :return None: None
    """
    ses = session.Session(args)
    with connection:
        while True:
            try:
                number_of_bytes = int(receive_chars(connection, 10))
                query = receive_chars(connection, number_of_bytes)
            except IOError:
                break

            logging.debug(f"Query received: '{query}'")

            answer = ses.handle_query(query)

            logging.debug(f"Answer generated: '{answer}'")
            logging.debug(f"Ids active: {ses.objects.keys()}")
            logging.debug(f"Modules active: {ses.mods.keys()}")
            logging.debug(f"Answer stack:{ses.answers}")

            # TODO Dynamic zero padding length
            if answer != "":
                answer = f"{len(answer):010}{answer}"
                connection.sendall(answer.encode(ENCODING))

    logging.info("Session closed")


def receive_chars(connection: socket.socket, number_of_bytes: int) -> str:
    """Reads an exact number of chars from a socket

    :param socket connection: socket to read from
    :param int number_of_bytes: number of bytes to read
    :return str: String of read chars
    """
    data = b"".join(receiver(connection, number_of_bytes))
    return data.decode(ENCODING)


def receiver(
    connection: socket.socket, number_of_bytes: int
) -> Generator[bytes, None, None]:
    """Generator that yields an exact number of bytes in chunks

    :param socket connection: socket to read from
    :param int number_of_bytes: number of bytes to read
    :return Generator[bytes, None, None]: Generator that yields chunks of
                                          bytes from the socket
    """
    while number_of_bytes > 0:
        chunk = connection.recv(number_of_bytes)
        if not chunk:
            raise IOError
        number_of_bytes -= len(chunk)
        yield chunk
