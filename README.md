dot.emacs.d
===========

emacs configuration

## To use:

    mv -f ~/.emacs.d ~/.emacs.d.disabled
    mv -f ~/.emacs ~/.emacs.disabled
    git clone https://github.com/karrick/dot.emacs.d.git
    mv -f dot.emacs.d ~/.emacs.d

## Create Desktop Launcher for a single user:

	install -D -m 644 emacsclient.desktop ~/.local/share/applications

## Create Desktop Launcher for all users:

	sudo install -m 644 emacsclient.desktop /usr/share/applications
