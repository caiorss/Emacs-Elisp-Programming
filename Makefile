all: html md

copy:
	rsync  -arvHP --exclude=.git/ \
	--exclude=*.elc --exclude=history \
	--exclude=saveplace \
	--exclude="~*" \
	--exclude="*~" \
	--exclude="#*" \
	--exclude="*#" \
	--exclude=*.cache \
	~/.emacs.d/  dotemacs/

index:
	doctoc README.md

html:
	emacs README.org --batch -l ~/.emacs.d/init.el -f org-html-export-to-html --kil
	mv README.html index.html

# Github Flavoured Markdown.
md:
	emacs README.org --batch -l ~/.emacs.d/init.el -f org-gfm-export-to-markdown --kill
	emacs Emacs_Snippets.org --batch -l ~/.emacs.d/init.el -f org-gfm-export-to-markdown --kill


index:
	doctoc README.md

html:
	emacs README.org --batch -l ~/.emacs.d/init.el -f org-html-export-to-html --kil
	emacs Emacs_Snippets.org --batch -l ~/.emacs.d/init.el -f org-html-export-to-html --kil
	mv README.html index.html

# Github Flavoured Markdown.
md:
	emacs README.org --batch -l ~/.emacs.d/init.el -f org-gfm-export-to-markdown --kill
	# emacs README.org --batch -f org-md-export-to-markdown --kil

html_md: md
	grip README.md --gfm --export ./index.html

clean:
	rm -rf *.html *.elc

#.org --batch -l ~/.emacs.d/init.el -f org-gfm-export-to-markdown --kill

	# emacs README.org --batch -f org-md-export-to-markdown --kil

html_md: md
	grip README.md --gfm --export ./index.html

clean:
	rm -rf *.html *.elc

