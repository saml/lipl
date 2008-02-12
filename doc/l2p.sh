#!/bin/bash

REQUIRED="pdflatex"
SYNOPSIS="Usage: $0 [-v] document.tex"

err ()  { echo "$*"   | fold -s -w ${COLUMNS:-110} >&2; }
if (( $# < 1 )); then
    err "$SYNOPSIS"
    exit 1
fi

texname="${1%.tex}"
echo "texname: $texname"

THIS="${0##*/}"
THIS_TEMPDIR=$(mktemp -d -t "$THIS.XXXXXXXX") || exit 1
readonly THIS_TEMPDIR
echo "THIS_TEMPDIR: $THIS_TEMPDIR"

trap 'exitcode=$?
      [ -z "$THIS_TEMPDIR" ] || rm -rf "$THIS_TEMPDIR"
      exit $exitcode' 0 1 2 3 13 15

logfile="$THIS_TEMPDIR/log"

(
    origdir=$(pwd)
    cd $THIS_TEMPDIR
    TEXINPUTS=$origdir:$TEXINPUTS:
    export TEXINPUTS
    finished=no
    runs=0
    while [ $finished = "no" ]; do
        pdflatex -interaction=batchmode "$texname.tex" > /dev/null || {
            errcode=$?
            cat "$texname.log"
            err "${THIS}: pdfLaTeX failed with error code $errcode"
            exit $errcode
        }
        if [ $runs -lt 3 ] && grep -q "LaTeX Warning: There were undefined references." "$texname.log"; then
            runs=$(($runs + 1))
            if grep -q "LaTeX Warning:.*[Cc]itation" "$texname.log"; then
                bibtex "$texname" 2>&1 >bibtex.err
                if [ $runs -gt 2 ]; then
                    if grep -q "error message" bibtex.err ||
                        grep -q "Warning" bibtex.err; then
                        cat bibtex.err >&2
                    fi
                fi
            fi
        else
            finished=yes
        fi
    done
) || exit $?

mv -f "$THIS_TEMPDIR/$texname.pdf" "$texname.pdf"
echo "$texname.pdf created"
