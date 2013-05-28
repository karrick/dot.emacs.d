## dot.emacs.d

* Minimalistic.

* Uses built-in emacs package manager to specify which optional
packages to install.

### Grab and install a copy

```bash
mv -f ~/.emacs.d ~/.emacs.d.disabled
mv -f ~/.emacs ~/.emacs.disabled
cd ~
git clone https://github.com/karrick/dot.emacs.d
mv -f ~/dot.emacs.d ~/.emacs.d
```

### Create Desktop Launcher

##### For a single user

```bash
install -D -m 644 emacsclient.desktop ~/.local/share/applications
```

##### For all users

```bash
sudo install -m 644 emacsclient.desktop /usr/share/applications
```
