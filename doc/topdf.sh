#!/bin/bash

rst2latex.py --documentoptions=10pt,letterpaper\
             --hyperlink-color=0\
             --stylesheet=default.tex\
             --section-numbering lipl.txt > lipl.tex

bash l2p.sh lipl.tex
