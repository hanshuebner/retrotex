#!/bin/sh

fswatch -0 rafi-edit-keyboard.xml make-svg.xsl styles.css | xargs -0 -n1 -I{} saxon -o:generated.svg rafi-edit-keyboard.xml make-svg.xsl
