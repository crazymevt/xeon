#!/usr/bin/env python3
"""Native, headless terminal emulator and real PTY/ConPTY regression checks."""
import ctypes as C
import os
from pathlib import Path
import sys
import time

root = Path(__file__).resolve().parent.parent
name = 'krypton_terminal.dll' if os.name == 'nt' else ('libkrypton_terminal.dylib' if sys.platform == 'darwin' else 'libkrypton_terminal.so')
paths = [root / 'build/terminal/lib' / name, root / 'build/terminal/lib/Release' / name]
lib = C.CDLL(str(next(p for p in paths if p.exists())))
def bind(name, result, *args):
    f = getattr(lib, 'kte_' + name)
    f.restype = result
    f.argtypes = args
    return f
ptr, integer = C.c_void_p, C.c_int
create = bind('create', ptr, integer, integer)
destroy = bind('destroy', None, ptr)
feed = bind('feed', None, ptr, C.c_char_p, integer)
snapshot = bind('snapshot', ptr, ptr)
length = bind('length', integer, ptr)
styles = bind('styles', ptr, ptr)
status = bind('status', integer, ptr)
start = bind('start', integer, ptr, C.c_char_p, C.c_char_p)
poll = bind('poll', integer, ptr)
paste = bind('paste', integer, ptr, C.c_char_p, integer)
key = bind('key', integer, ptr, integer, integer, integer)
resize = bind('resize', integer, ptr, integer, integer)
cursor = bind('cursor', integer, ptr)
history = bind('history_lines', integer, ptr)
mouse = bind('mouse_mode', integer, ptr)
checks = 0

def check(value):
    global checks
    checks += 1
    assert value, f'terminal check {checks} failed'

def text(t):
    p = snapshot(t)
    n = length(t)
    result = C.string_at(p, n).decode('utf-8')
    check(len(C.string_at(styles(t), n)) == len(result.encode('utf-8')))
    return result

def send(t, s):
    b = s.encode('utf-8') if isinstance(s, str) else s
    feed(t, b, len(b))

def wait_for(t, predicate, timeout=8):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        check(poll(t) <= 65536)
        s = text(t)
        if predicate(s):
            return s
        time.sleep(.015)
    raise AssertionError('terminal timed out: ' + repr(s[-1500:]))

check(bind('abi', integer)() == 2)
command_running = bind('command_running', integer, ptr)
check(not create(1, 80))
t = create(5, 20)
try:
    check(bool(t))
    send(t, 'hello\rXY')
    check(text(t).startswith('XYllo'))
    send(t, '\x1b[2J\x1b[H\x1b[31;1mred\x1b[0m plain')
    check(text(t).startswith('red plain'))
    check(C.string_at(styles(t), length(t))[0] != C.string_at(styles(t), length(t))[4])
    # Fragmented UTF-8, combining marks and double-width CJK cells.
    send(t, '\x1b[2J\x1b[H')
    b = 'λ界é😀'.encode()
    for byte in b:
        send(t, bytes([byte]))
    check(text(t).startswith('λ界é😀'))
    send(t, '\x1b[?1049h\x1b[Halternate')
    check('alternate' in text(t))
    check(history(t) == 0)
    send(t, '\x1b[?1049l')
    check(text(t).startswith('λ界é😀'))
    send(t, '\x1b[?1002h')
    check(mouse(t) == 1)
    send(t, '\x1b[?1002l')
    check(mouse(t) == 0)
    send(t, '\x1b[2J\x1b[H' + ''.join(f'line-{i}\r\n' for i in range(2500)))
    check(history(t) == 1000)
    s = text(t)
    check('line-2499' in s)
    check('line-0 ' not in s)
    check(len(s) < 25000)
    check(0 <= cursor(t) < len(s.encode()))
    check(resize(t, 10, 40) == 1)
    check(resize(t, 10000, 40) == 0)
    # Untrusted terminal output must not set the host clipboard or open URLs.
    send(t, '\x1b]52;c;ZXZpbA==\x07\x1b]8;;https://example.invalid\x07link\x1b]8;;\x07')
    check('link' in text(t))
finally:
    destroy(t)

# Real OS terminal, including working directory with spaces and Unicode.
import tempfile
with tempfile.TemporaryDirectory(prefix='krypton terminal λ ') as directory:
    t = create(24, 80)
    try:
        shell = b'cmd.exe' if os.name == 'nt' else b'/bin/sh'
        check(start(t, directory.encode('utf-8'), shell) == 1)
        if os.name == 'nt':
            wait_for(t, lambda s: directory.rsplit(' ', 1)[-1] + '>' in s)
        command = ('echo krypton-conpty-rea^dy' if os.name == 'nt' else
                   "test -t 0 && test -t 1 && printf 'krypton-%s\\n' 'pty-ready'")
        b = command.encode()
        check(paste(t, b, len(b)) == 1)
        check(key(t, 1, 0, 0) == 1)
        wait_for(t, lambda s: ('krypton-conpty-ready' if os.name == 'nt' else 'krypton-pty-ready') in s)
        wait_for(t, lambda s: not command_running(t))
        check(resize(t, 30, 100) == 1)
        if os.name != 'nt':
            b = b'stty size'
            check(paste(t, b, len(b)) == 1)
            key(t, 1, 0, 0)
            wait_for(t, lambda s: '30 100' in s)
            # Ctrl+C is sent through the terminal, not delivered to the editor.
            b = b'sleep 30'
            paste(t, b, len(b));key(t, 1, 0, 0)
            time.sleep(.1)
            check(command_running(t) == 1)
            key(t, 0, ord('c'), 4)
            b = b"printf 'interrupt-%s\\n' 'ok'"
            paste(t, b, len(b));key(t, 1, 0, 0)
            wait_for(t, lambda s: 'interrupt-ok' in s)
            wait_for(t, lambda s: not command_running(t))
        b = b'exit 7'
        check(paste(t, b, len(b)) == 1)
        key(t, 1, 0, 0)
        wait_for(t, lambda s: status(t) != -1)
        check(status(t) == 7)
        check(command_running(t) == 0)
        check(resize(t, 20, 60) == 1)  # exited sessions still move between docks
        check(key(t, 1, 0, 0) == 0)
    finally:
        before = time.monotonic()
        destroy(t)
        check(time.monotonic() - before < 3)

# Closing a live, busy session must remain bounded and reap its shell.
for _ in range(4):
    t = create(24, 80)
    check(start(t, str(root).encode(), b'cmd.exe' if os.name == 'nt' else b'/bin/sh') == 1)
    if os.name != 'nt':
        b = b'yes output'
        paste(t, b, len(b));key(t, 1, 0, 0)
    time.sleep(.05)
    before = time.monotonic()
    destroy(t)
    check(time.monotonic() - before < 3)
print(f'{checks} native terminal checks passed on {sys.platform}.')
