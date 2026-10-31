#!/usr/bin/env python3
"""Write layers.pdf, a one-page PDF with optional content groups.

Usage: python3 layers.py [OUTPUT]   (default: layers.pdf next to this file)

The page shows one coloured box per layer.  The layer panel order is

  Base       (on,  red box)
  Answers    (off, green box)
    Hints    (on,  blue box, nested under Answers)
  Language   (a label, not a layer)
    English  (on,  black box)  \\ a radio button group:
    Deutsch  (off, yellow box) /  showing one hides the other

and a link in the top left corner toggles Answers (a SetOCGState
action).  The file is written by hand rather than with LaTeX so that
it stays tiny and its layer tree is exactly what the tests expect.
"""

import os
import sys

# (object number, title, initially visible, fill colour, box x)
LAYERS = [
    (4, "Base", True, "1 0 0", 20),
    (5, "Answers", False, "0 1 0", 60),
    (6, "Hints", True, "0 0 1", 100),
    (7, "English", True, "0 0 0", 140),
    (8, "Deutsch", False, "1 1 0", 180),
]


def content():
    ops = []
    for num, _, _, colour, x in LAYERS:
        ops.append("/OC /L%d BDC %s rg %d 100 30 30 re f EMC"
                   % (num, colour, x))
    # An always visible frame around the link area.
    ops.append("0.5 G 10 160 40 30 re S")
    return "\n".join(ops).encode("latin-1")


def ref(num):
    return "%d 0 R" % num


def objects():
    on = [ref(n) for n, _, v, _, _ in LAYERS if v]
    off = [ref(n) for n, _, v, _, _ in LAYERS if not v]
    ocgs = " ".join(ref(n) for n, _, _, _, _ in LAYERS)
    stream = content()
    props = " ".join("/L%d %s" % (n, ref(n)) for n, _, _, _, _ in LAYERS)
    objs = {
        1: ("<< /Type /Catalog /Pages 2 0 R"
            " /OCProperties << /OCGs [%s]"
            " /D << /Name (Default) /ON [%s] /OFF [%s]"
            " /Order [4 0 R 5 0 R [6 0 R] [(Language) 7 0 R 8 0 R]]"
            " /RBGroups [[7 0 R 8 0 R]] >> >> >>"
            % (ocgs, " ".join(on), " ".join(off))),
        2: "<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
        3: ("<< /Type /Page /Parent 2 0 R /MediaBox [0 0 220 200]"
            " /Resources << /Properties << %s >> >>"
            " /Contents 9 0 R /Annots [10 0 R] >>" % props),
        9: (b"<< /Length %d >>\nstream\n" % len(stream)
            + stream + b"\nendstream"),
        10: ("<< /Type /Annot /Subtype /Link /Rect [10 160 50 190]"
             " /Border [0 0 0]"
             " /A << /S /SetOCGState /State [/Toggle 5 0 R] >> >>"),
    }
    for num, title, _, _, _ in LAYERS:
        objs[num] = "<< /Type /OCG /Name (%s) >>" % title
    return objs


def write(path):
    out = bytearray(b"%PDF-1.5\n")
    objs = objects()
    offsets = {}
    for num in sorted(objs):
        body = objs[num]
        if isinstance(body, str):
            body = body.encode("latin-1")
        offsets[num] = len(out)
        out += b"%d 0 obj\n" % num + body + b"\nendobj\n"
    size = max(objs) + 1
    xref = len(out)
    out += b"xref\n0 %d\n0000000000 65535 f \n" % size
    for num in range(1, size):
        out += b"%010d 00000 n \n" % offsets[num]
    out += (b"trailer\n<< /Size %d /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n"
            % (size, xref))
    with open(path, "wb") as f:
        f.write(out)


if __name__ == "__main__":
    write(sys.argv[1] if len(sys.argv) > 1
          else os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "layers.pdf"))
