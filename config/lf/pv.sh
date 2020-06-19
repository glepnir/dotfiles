#!/bin/bash
unset COLORTERM
case "$1" in
    *.tar*) tar tf "$2";;
    *.zip) unzip -l "$1";;
    *.rar) unrar l "$1";;
    *.7z) 7z l "$1";;
    *.pdf) pdftotext "$1" -;;
    *) bat --plain --color=always "$1";;
esac

