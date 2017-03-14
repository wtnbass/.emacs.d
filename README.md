# .emacs.d

![screenshot](https://raw.githubusercontent.com/wtnbass/.emacs.d/images/startup-screenshot.png)
## Instllation

install cask

```
curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
```

install .emacs.d

```
cd
git clone git@github.com:wtnbass/.emacs.d.git
cd .emacs.d
cask install
```

## Go

you need to install these packages:

```
go get github.com/rogpeppe/godef
go get -u github.com/nsf/gocode
go get github.com/golang/lint/golint
go get github.com/kisielk/errcheck
```
