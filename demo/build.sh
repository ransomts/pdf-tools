#!/bin/bash
# Build the feature tour: tour.pdf and demo-slides.pdf, plus three blank
# copies of the tour in batch/ for the CSV batch fill.  Run it again
# any time for fresh copies; it overwrites them.
#
# Needs lualatex and pdflatex (TeX Live 2025 or later, for the tagged
# PDF), qpdf, pdftotext and python3.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

cp "$here"/tour.tex "$here"/demo-slides.tex "$work"
cd "$work"

# Three runs: labels, then the page numbers in the contents, then the
# structure tree settle.
for _ in 1 2 3; do
  lualatex -interaction=nonstopmode -halt-on-error tour.tex >/dev/null \
    || { tail -30 tour.log; exit 1; }
done
for _ in 1 2; do
  pdflatex -interaction=nonstopmode -halt-on-error demo-slides.tex >/dev/null \
    || { tail -30 demo-slides.log; exit 1; }
done

# What LaTeX can't write: the empty signature field on the signatures
# page, and roster.csv as a document-level attachment.
sigpage=$(pdftotext tour.pdf - | awk -v RS='\f' '/Signatures and certificates/ && !/Contents/ {print NR; exit}')
python3 -I "$here/sigfield.py" tour.pdf signed-field.pdf "$sigpage"
qpdf signed-field.pdf tour-final.pdf \
  --add-attachment "$here/roster.csv" --mimetype=text/csv \
  --description="The roster the form on page 5 can be filled from" --
qpdf --check tour-final.pdf >/dev/null

install -m 644 tour-final.pdf "$here/tour.pdf"
install -m 644 demo-slides.pdf "$here/demo-slides.pdf"

mkdir -p "$here/batch"
for who in ada grace alan; do
  install -m 644 tour-final.pdf "$here/batch/$who.pdf"
done
cat > "$here/batch/roster.csv" <<'EOF'
file,name,email,course,role,consent
ada.pdf,Ada Lovelace,ada@example.edu,CPSC 1010,Student,yes
grace.pdf,Grace Hopper,grace@example.edu,CPSC 2120,TA,yes
alan.pdf,Alan Turing,alan@example.edu,CPSC 4300,Instructor,no
EOF

echo "Built $here/tour.pdf (signature field on page $sigpage), demo-slides.pdf and batch/."
