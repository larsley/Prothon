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
import threading
import logging
import argparse


import connection

HOST = "127.0.0.1"
PORT = 44644


def main():
    args = argument_parsing()
    logging_format = (
        "%(asctime)s.%(msecs)03d %(threadName)-10s "
        "%(levelname)-8s %(message)s "
    )
    logging.basicConfig(
        format=logging_format,
        datefmt="%Y-%m-%d %H:%M:%S",
        filename="prothon.log",
        encoding="utf-8",
        level=logging.WARN,
    )
    logging.info("Server started")
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((args.address, args.port))
        sock.listen()
        while True:
            conn, addr = sock.accept()
            thread = threading.Thread(
                target=connection.connection_loop, args=(conn, args)
            )
            thread.name = thread.name.removesuffix(" (connection_loop)")
            logging.info(f"Connected by {addr} starting {thread.name}")
            thread.start()


def argument_parsing():
    parser = argparse.ArgumentParser(
        description="Server executing arbitrary Python function calls"
    )
    parser.add_argument(
        "-p", "--port", default=PORT, type=int, help="port to listen on"
    )
    parser.add_argument(
        "-a", "--address", default=HOST, help="IP address of the server"
    )
    parser.add_argument(
        "-m",
        "--module",
        action="extend",
        nargs="+",
        help="adds the specified module(s) to the whitelist",
    )
    parser.add_argument(
        "-b",
        "--buildin",
        action="extend",
        nargs="+",
        help="adds the specified built-in(s) to the whitelist",
    )
    return parser.parse_args()


if __name__ == "__main__":
    main()
