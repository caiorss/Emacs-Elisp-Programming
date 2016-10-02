all: html

html:
	emacs --batch -q -l build.el --kill 

clean:
	rm -rf dist/*.html dist/*.html~

