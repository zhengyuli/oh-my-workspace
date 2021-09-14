# Oh My Workspace Configurations

This guide includes all kinds of configurations of daily used tools on mac.

## Command Line Tools

The Command Line Tool package gives Mac terminal users many commonly used tools, utilities, compilers, and many other useful commands that are usually found in default linux installations.

### Installation

Use xcode-select to download and install:

```
$ xcode-select --install
```

## Homebrew

**[Homebrew](https://brew.sh/)** the most popular package manager on mac.

### Installation

Install homebrew:

```
$ /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Add `/usr/local/bin` to the $PATH:

```
$ echo 'export PATH="/usr/local/bin:$PATH"' >> ~/.zshrc
```

Self-check and update brew:

```
$ brew doctor && brew update
```

## Iterm2

**[iTerm2](https://www.iterm2.com/)** is a replacement for Terminal and the successor to iTerm.

It brings the terminal into the modern age with features you never knew you always wanted.

### Installation

Use Homebrew to download and install:

```
$ brew cask install iterm2
```

### Customization

#### Theme

Download Solarized theme for iTerm2:

```
$ git clone https://github.com/altercation/solarized
```

Import Solarized dark theme through `iTerm2->Preferences->Profiles->Colors->Color Presets->Import`.

Select the **Solorized Dark** theme through `iTerm2->Preferences->Profiles->Colors->Color Presets->Solorized Dark`.

#### Key Bindings

Configure shortcuts through `iTerm2->Preferences->Keys->Key Mappings` panel.

| Action       | Key Mapping |
|:-------------|:------------|
| Previous Tab | ⇧⌘P         |
| Next Tab     | ⇧⌘N         |

## Zsh + Oh My Zsh

**[Z shell](http://zsh.sourceforge.net/)** (also known as zsh) is a Unix shell that is built on top of bash (the default shell for macOS).

**[Oh My Zsh](https://ohmyz.sh/)** is an open source, community-driven framework for managing your zsh configuration. It comes with a bunch of features out of the box and improves your terminal experience.

### Installation

Use Homebrew to download and install:

```
$ brew install zsh
```

Install Oh My Zsh:

```
$ sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
$ chsh -s $(which zsh)
```

### Customization

The out-of-the-box configuration is usable but you probably want to customise it to suit your needs.

The Official [Wiki](https://github.com/robbyrussell/oh-my-zsh/wiki) contains a lot of useful information if you want to deep dive into what you can do with Oh My Zsh, but we'll cover the basics here.

#### Plugins

Standard plugins can be found in `~/.oh-my-zsh/plugins/*`, custom plugins may be added to `~/.oh-my-zsh/custom/plugins/`.

Add plugins to your shell by adding the name of the plugin to the plugin array in your `~/.zshrc`.

Download custom plugins:

```
$ git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
$
$ git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
```

Add daily used plguins:

```
# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
   fzf
   git
   docker
   iterm2
   autojump
   history
   zsh-autosuggestions
   zsh-syntax-highlighting
)

# Key bindings for autosuggestions plugin
bindkey '^],' autosuggest-accept
```

#### Themes

Changing theme is as simple as changing a string in your configuration file.

```
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="sonicradish"
```

#### env.sh

Create a new `~/.env.sh` script with the following contents:

```
#!/bin/zsh

# Disable homebrew auto update
export HOMEBREW_NO_AUTO_UPDATE="true"
```

import `~/.env.sh` script in `~/.zshrc`:

```
$ source ~/.env.sh
```

## Golang

Go is an open source programming language.

### Installation

Download the latest version from [https://golang.org/dl/]() and install manually.

### Customization

Create Golang projects workspace:

```
$ mkdir -p $HOME/MyGoProjects
```

Append the following settings to your `~/.env.sh` script:

```
# Settings for golang
export GOPATH=$HOME/MyGoProjects

# Update $PATH
export PATH=$GOPATH/bin:$HOME/bin:/usr/local/bin:$PATH
```

Install golang related dependencies:

```
$ go get golang.org/x/tools/cmd/godoc
$ go get github.com/rogpeppe/godef
$ go get github.com/nsf/gocode
```

## Pyenv

pyenv is a very popular python management tool.

It lets you easily switch between multiple versions of Python.

### Installation

Use Homebrew to download and install:

```
$ brew install pyenv
$
# Install your prefered python version
$ pyenv install 2.7.15
$ pyenv install 3.7.0
```

### Customization

Append the following settings to your `~/.env.sh` script:

```
# Settings for pyenv
eval "$(pyenv init -)"
```

## pyenv-virtualenvwrapper

### Installation

Using homebrew to download and install:

```
$ brew install pyenv-virtualenvwrapper
```

### Customization

Append the following settings to your `~/.env.sh` script:

```
# Settings for pyenv-virtualenvwrapper
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

# Activate virtualenvwrapper
pyenv shell 2.7.15
pyenv virtualenvwrapper
```

## Python

macOS, like Linux, ships with Python already installed.

For not messing with the system Python (some system tools rely on it, etc.), we need install our own version(s).

### Installation

Using pyenv to download and install:

```
$ pyenv install 2.7.15
$ pyenv global 2.7.15
$ pyenv rehash
```

### Emacs

Emacs is an extensible, customizable, free/libre text editor — and more.

### Installation

Using homebrew to download and install:

```
$ brew tap railwaycat/emacsmacport
$ brew cask install emacs-mac
$ brew untap railwaycat/emacsmacport
```

Installing emacs related dependencies:

```
# Dependencies for emacs multimedia
$ brew install mplayer
$
# Dependencies for emacs w3m
$ brew install w3m
$
# Dependencies for emacs easypg
$ brew install gnupg
$
# Dependencies for emacs markdown
$ brew install markdown
$ brew install pandoc
$
# Dependencies for emacs dump-jump
$ brew install ripgrep
$
# Dependencies for emacs c&c++ mode (please install the latest version)
$ brew install rtags
$
# Dependencies for emacs golang mode
$ go get github.com/rogpeppe/godef
$ go get golang.org/x/tools/cmd/gorename
$ go get golang.org/x/tools/cmd/guru
```

#### Customization

Install Source Code Pro font from: https://github.com/adobe-fonts/source-code-pro

Append the following settings to your `~/.virtualenvs/postmkvirtualenv`:

```
# Dependencies for emacs python-mode
pip install yapf
pip install jedi==0.17.2
pip install epc
pip install pylint

# Other tools
pip install ipython
```

Setup emacs configurations:

```
$ ./emacs/install.sh
```
