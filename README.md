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

### Configure

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
$ sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
$ chsh -s $(which zsh)
```

### Configure

The out-of-the-box configuration is usable but you probably want to customise it to suit your needs.

The Official [Wiki](https://github.com/ohmyzsh/ohmyzsh/wiki) contains a lot of useful information if you want to deep dive into what you can do with Oh My Zsh, but we'll cover the basics here.

#### Themes

Changing theme is as simple as changing a string in your configuration file.

```
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="agnoster"
```

#### Plugins

Standard plugins can be found in `~/.oh-my-zsh/plugins/*`, custom plugins may be added to `~/.oh-my-zsh/custom/plugins/`.

Add plugins to your shell by adding the name of the plugin to the plugin array in your `~/.zshrc`.

Download custom plugins:

```
$ brew install fzf
$
$ git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
$ brew install autosuggestions
$
$ git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
```

Configure daily used plguins:

```
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#a9a9a9"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    history
    fzf
    autojump
    zsh-autosuggestions
    zsh-syntax-highlighting
    brew
    git
    docker
    web-search
)
```

#### Others

Append the following settings to your configuration file.

```
# Disable homebrew auto update
export HOMEBREW_NO_AUTO_UPDATE="true"
```

## LLVM

The LLVM Project is a collection of modular and reusable compiler and toolchain technologies.

### Installation

Use Homebrew to download and install:

```
$ brew install llvm
```

### Configure
Append the following settings to your `~/.zshrc`:

```
$ echo 'export PATH="$(brew --prefix llvm)/bin:$PATH"' >> ~/.zshrc
```

## Golang

Go is an open source programming language.

### Installation

Download the latest version from [https://golang.org/dl/]() and install manually.

### Configure

Create Golang projects workspace:

```
$ mkdir -p $HOME/MyGoProjects
```

Append the following settings to your `~/.zshrc`:

```
# Path to your go projects
export GOPATH=$HOME/MyGoProjects

# Update $PATH
export PATH=$GOPATH/bin:$PATH
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
$ pyenv install 3.8.10
```

### Configure

Append the following settings to your `~/.zshrc`:

```
# Initialize pyenv
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
```

## pyenv-virtualenvwrapper

### Installation

Using git to download and install:

```
$ git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git $(pyenv root)/plugins/pyenv-virtualenvwrapper
```

### Configure

Append the following settings to your `~/.zshrc`:

```
# To get virtualenvwrapper to create a virtual environment using 
# pyvenv instead of virtualenv.
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"

# Activate shell python
pyenv shell 3.8.10

# Activate virtualenvwrapper
pyenv virtualenvwrapper
```

## Python

macOS, like Linux, ships with Python already installed.

For not messing with the system Python (some system tools rely on it, etc.), we need install our own version(s).

### Installation

Using pyenv to download and install:

```
$ pyenv install 3.8.10
$ pyenv global 3.8.10
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
# Dependencies for emacs easypg
$ brew install gnupg
$
# Dependencies for emacs ag
$ brew install ag
$
# Dependencies for emacs w3m
$ brew install w3m
$
# Dependencies for emacs c&c++ lsp mode and dap mode
# Please make sure **clangd** and **lldb-vscode** have been installed,
# For Mac please follow llvm installation section described above.
# For other linux distribution e.g. ubuntu please follow the following
# instructions to install them.
$ sudo apt-get install clang clang-tools
$
# Dependencies for emacs python lsp mode and dap mode
$ pip install "python-lsp-server[all]"
$ pip install "ptvsd>=4.2"
$
# Dependencies for emacs golang mode
$ go get github.com/rogpeppe/godef
$ go get golang.org/x/tools/cmd/gorename
$ go get golang.org/x/tools/cmd/guru
$
# Dependencies for emacs markdown
$ brew install markdown
$ brew install pandoc
```

#### Configure

Install Source Code Pro font from: https://fonts.google.com/specimen/Source+Code+Pro

Install Source Serif Pro font from: https://fonts.google.com/specimen/Source+Serif+Pro

Setup emacs configurations:

```
$ ./emacs/install.sh
```
