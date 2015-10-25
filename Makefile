

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

index:
	doctoc README.md

html: index
	grip README.md --gfm --export ./RAEDME.html

