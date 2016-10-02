all: html


html:
	mkdir -p dist 
	emacs --batch -q -l build.el --kill
	mv dist/README.html dist/index.html 
	cp -r -v images dist/images
	cp -r *.org  dist/

browse:
	firefox dist/index.html

clean:
	rm -rf dist/*.html dist/*.html~

