% dot.emacs.d
% Karrick S. McDermott
% Wed May 1 11:59:51 2013 -0700

## Rationale

* Minimalistic.

* Uses built-in emacs package manager to specify which optional
packages to install.

## Grab and install a copy

```{.bash .numberLines}
mv -f ~/.emacs.d ~/.emacs.d.disabled
mv -f ~/.emacs ~/.emacs.disabled
cd ~
git clone https://github.com/karrick/dot.emacs.d
mv -f ~/dot.emacs.d ~/.emacs.d
cd ~/.emacs.d
make compile
```

During startup, initialization will prompt user to install any
required packages that are not yet installed that are specified in the
initialization.

## Create Desktop Launcher

#### For a single user

```{.bash .numberLines}
install -D -m 644 emacsclient.desktop ~/.local/share/applications
```

#### For all users

```{.bash .numberLines}
sudo install -m 644 emacsclient.desktop /usr/share/applications
```
