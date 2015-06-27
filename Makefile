

all: html

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

html:
	grip README.md --gfm --export ./RAEDME.html

