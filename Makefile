PREFIX = /usr/local/bin

install:
	cp .xinitrc ~/.xinitrc
	sudo cp emacs-wm $(PREFIX)/emacs-wm

uninstall:
	sudo rm $(PREFIX)/emacs-wm
