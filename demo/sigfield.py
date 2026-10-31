#!/usr/bin/env python3
"""Add an empty signature field `approval' to a page of a PDF.

Usage: python3 sigfield.py IN.pdf OUT.pdf PAGE

hyperref can't write a signature field, so build.sh adds it here with
qpdf's JSON update mode.  The field goes in the box that tour.tex
leaves for it: the frame above the grey label "empty signature field",
found with pdftotext.
"""

import json
import os
import re
import subprocess
import sys


def label_box(pdf, page):
    """Return (x0, x1, y_top) of the grey label, in PDF points."""
    html = subprocess.run(
        ["pdftotext", "-bbox", "-f", str(page), "-l", str(page), pdf, "-"],
        capture_output=True, text=True, check=True).stdout
    size = re.search(r'<page width="[\d.]+" height="([\d.]+)"', html)
    if not size:
        sys.exit("sigfield.py: pdftotext gave no page size")
    height = float(size.group(1))
    words = re.findall(r'<word xMin="([\d.]+)" yMin="([\d.]+)" '
                       r'xMax="([\d.]+)" yMax="([\d.]+)">([^<]*)</word>', html)
    # The label is the lowest line on the page that says "signature".
    hits = [w for w in words if w[4] == "signature"]
    if not hits:
        sys.exit("sigfield.py: no label on page %d" % page)
    y = max(float(w[3]) for w in hits)
    line = [w for w in words if abs(float(w[3]) - y) < 2]
    return (min(float(w[0]) for w in line), max(float(w[2]) for w in line),
            height - min(float(w[1]) for w in line))


def main(src, dst, page):
    x0, x1, label_top = label_box(src, page)
    centre = (x0 + x1) / 2
    # The frame is 8.5cm by 2.4cm (241 by 68pt), just above the label.
    rect = [centre - 118, label_top + 4, centre + 118, label_top + 70]

    doc = json.loads(subprocess.run(["qpdf", "--json", src],
                                    capture_output=True, text=True,
                                    check=True).stdout)
    objs = doc["qpdf"][1]
    trailer = objs["trailer"]["value"]
    catalog_ref = trailer["/Root"]
    catalog = objs["obj:" + catalog_ref]["value"]
    page_ref = doc["pages"][page - 1]["object"]
    page_dict = objs["obj:" + page_ref]["value"]

    new = max(int(k.split(":")[1].split()[0])
              for k in objs if k.startswith("obj:")) + 1
    new_ref = "%d 0 R" % new
    update = {"obj:" + new_ref: {"value": {
        "/Type": "/Annot", "/Subtype": "/Widget", "/FT": "/Sig",
        "/T": "u:approval", "/TU": "u:Approval signature", "/F": 4,
        "/P": page_ref, "/Rect": rect,
        "/MK": {"/BC": [0, 0, 1]}}}}

    def add_to_array(holder_ref, holder, key):
        """Append NEW_REF to HOLDER's array KEY, direct or indirect."""
        value = holder.get(key, [])
        if isinstance(value, str):      # an indirect array
            array = objs["obj:" + value]["value"] + [new_ref]
            update["obj:" + value] = {"value": array}
        else:
            holder = dict(holder, **{key: value + [new_ref]})
            update["obj:" + holder_ref] = {"value": holder}
        return holder

    add_to_array(page_ref, page_dict, "/Annots")
    acroform = catalog["/AcroForm"]
    if isinstance(acroform, str):
        add_to_array(acroform, objs["obj:" + acroform]["value"], "/Fields")
    else:
        fields = acroform["/Fields"]
        if isinstance(fields, str):
            add_to_array(None, {"/Fields": fields}, "/Fields")
        else:
            catalog = dict(catalog, **{"/AcroForm": dict(
                acroform, **{"/Fields": fields + [new_ref]})})
            update["obj:" + catalog_ref] = {"value": catalog}

    upd = json.dumps({"qpdf": [{"jsonversion": 2, "pushedinheritedpageresources": False,
                                "calledgetallpages": False, "maxobjectid": new},
                               update]})
    upd_file = dst + ".json"
    with open(upd_file, "w") as f:
        f.write(upd)
    subprocess.run(["qpdf", src, dst, "--update-from-json=" + upd_file],
                   check=True)
    os.remove(upd_file)


if __name__ == "__main__":
    if len(sys.argv) != 4:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], int(sys.argv[3]))
