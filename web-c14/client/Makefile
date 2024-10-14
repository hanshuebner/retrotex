
all: rafi-editing-keyboard.svg

DEPS=rafi-editing-keyboard.xml make-svg.xsl svg-styles.css

rafi-editing-keyboard.svg: $(DEPS)
	saxon -o:rafi-editing-keyboard.svg rafi-editing-keyboard.xml make-svg.xsl

develop-xslt: rafi-editing-keyboard.svg
	fswatch -0 $(DEPS) | xargs -0 -n1 -I{} make

run-server:
	python3 -m http.server 8000
