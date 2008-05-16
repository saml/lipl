#!/bin/bash

fn="$1"
name="${fn%.}"
rst2html.py "$fn" > "$name.html"

