#!/usr/bin/env python3
"""Групує звіт valgrind helgrind/drd за першим кадром стеку кожного конфлікту.

Використання: hgsummary.py <файл_логу> [кількість_рядків]
Для кожного «Possible data race» показує обидва доступи: поточний і попередній
(рядок «This conflicts with a previous ...»).
"""
import collections
import re
import sys

FRAME = re.compile(r"(?:at|by) 0x[0-9A-F]+: (\S+) \(([^)]*)\)")


def short(frame):
    func, loc = frame
    return f"{func[:60]} ({loc.split('/')[-1]})"


def main():
    text = open(sys.argv[1], encoding="utf-8", errors="replace").read()
    limit = int(sys.argv[2]) if len(sys.argv) > 2 else 40
    counts = collections.Counter()
    for block in re.split(r"==\d+== -{20,}\n", text):
        if "Possible data race" not in block:
            continue
        cur, _, prev = block.partition("This conflicts with a previous")
        cur_frames = FRAME.findall(cur)
        prev_frames = FRAME.findall(prev)
        key = (short(cur_frames[0]) if cur_frames else "?",
               short(prev_frames[0]) if prev_frames else "?")
        counts[key] += 1
    for (cur, prev), n in counts.most_common(limit):
        print(f"{n:4}  {cur}\n      <-> {prev}")
    print(f"Усього груп: {len(counts)}, конфліктів: {sum(counts.values())}")


if __name__ == "__main__":
    main()
