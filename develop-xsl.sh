#!/bin/sh

fswatch -0 rafi-editing-keyboard.xml make-svg.xsl styles.css \
	| xargs -0 -n1 -I{} saxon -o:rafi-editing-keyboard.svg rafi-editing-keyboard.xml make-svg.xsl
