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
	emacs README.org --batch -f org-html-export-to-html --kil

# Github Flavoured Markdown.
md:
	emacs README.org --batch -f org-gfm-export-to-markdown --kill
	# emacs README.org --batch -f org-md-export-to-markdown --kil

html_md: md
	grip README.md --gfm --export ./README.html

clean:
	rm -rf *.html *.elc

