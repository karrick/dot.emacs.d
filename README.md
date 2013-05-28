## dot.emacs.d

emacs configuration

### Grab a copy

```bash
mv -f ~/.emacs.d ~/.emacs.d.disabled
mv -f ~/.emacs ~/.emacs.disabled
cd ~
git clone https://github.com/karrick/dot.emacs.d
mv -f ~/dot.emacs.d ~/.emacs.d
```

### Create Desktop Launcher

##### for a single user

```bash
install -D -m 644 emacsclient.desktop ~/.local/share/applications
```

##### for all users

```bash
sudo install -m 644 emacsclient.desktop /usr/share/applications
```
