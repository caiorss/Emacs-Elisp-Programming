all: html


html:
	mkdir -p dist
	emacs --batch -q -l build.el --kill  ## Compile org-pages to utml 
	mv dist/README.html dist/index.html  ## Create dist/index.html (gh-pages repository)

    ## Prepare files for upload.
	cp -r -v images/* dist/images/
	cp -r *.org  dist/
	# cp -r theme/* dist/theme 
	cp -r -v codes/* dist/codes

browse:
	firefox dist/index.html

# Upload the dist/* to origin/gh-pages
upload:
	cd dist && \
		git add images  && \
		git add codes   && \
		git add theme   && \
		git add *.org   && \
		git add *.html  && \
		git status      && \
		git commit -a -m "Update pages" && \
		git push

# Send master branch  to origin/master
push:
	git push

clean:
	rm -rf dist/*.html dist/*.html~
