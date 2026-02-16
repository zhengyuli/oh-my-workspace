# Oh My Workspace Configurations

Welcome to the Oh-My-Workspace repository! This project provides a comprehensive workspace configuration to streamline your development workflow.

## Table of Contents

- [Quick Start](#quick-start)
- [Command Line Tools (Mac)](#command-line-tools-mac)
- [Homebrew (Mac)](#homebrew-mac)
- [iTerm2 (Mac)](#iterm2-mac)
- [Zsh + Oh My Zsh](#zsh--oh-my-zsh)
- [GnuPG](#gnupg)
- [Pass](#pass)
- [LLVM](#llvm)
- [Golang](#golang)
- [Rust](#rust)
- [Pyenv](#pyenv)
- [pyenv-virtualenvwrapper](#pyenv-virtualenvwrapper)
- [Python](#python)
- [Emacs](#emacs)
- [Vim](#vim)

---

## Quick Start

| Component | Setup Script       | Description                                     |
|-----------|--------------------|-------------------------------------------------|
| Emacs     | `./emacs/setup.sh` | Emacs configuration with LSP, completion, etc. |
| Vim       | `./vim/setup.sh`   | Vim configuration with modern editing features |

Each setup script will:

1. Create symbolic links to the configuration files
1. Display a comprehensive dependency installation guide

### Recommended Installation Order

1. Install Command Line Tools (Mac only)
2. Install Homebrew (Mac only)
3. Install iTerm2 (Mac only, optional)
4. Install Zsh + Oh My Zsh
5. Install language runtimes (Golang, Rust, Pyenv, etc.)
6. Run editor setup scripts (`emacs/setup.sh`, `vim/setup.sh`)

---

## Command Line Tools (Mac)

The Command Line Tool package provides Mac terminal users with commonly used tools, utilities, and compilers.

### Installation

```shell
xcode-select --install
```

---

## Homebrew (Mac)

[Homebrew](https://brew.sh/) is the most popular package manager on macOS.

### Installation

1. Install homebrew:

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

1. Add to `$PATH`:

```shell
echo 'export PATH="$(brew --prefix)/bin:$PATH"' >> ~/.zshrc
```

1. Self-check and update:

```shell
brew doctor && brew update
```

---

## iTerm2 (Mac)

[iTerm2](https://www.iterm2.com/) is a modern terminal replacement with advanced features.

### Installation

```shell
brew install --cask iterm2
```

### Configuration

#### Fonts

1. Install fonts from:
   - [Source Code Pro](https://www.fontsquirrel.com/fonts/source-code-pro)
   - [Source Serif Pro](https://www.fontsquirrel.com/fonts/source-serif-pro)
2. Select **Source Code Pro** via *iTerm2 → Preferences → Profiles → Text → Font*

#### Theme

1. Download Dracula theme:

```shell
git clone https://github.com/dracula/iterm.git
```

1. Import via *iTerm2 → Preferences → Profiles → Colors → Color Presets → Import*
1. Select **Dracula** theme

#### Key Bindings

| Action       | Key Mapping |
|--------------|-------------|
| Previous Tab | ⇧⌘P         |
| Next Tab     | ⇧⌘N         |

---

## Zsh + Oh My Zsh

[Z shell](http://zsh.sourceforge.net/) is a Unix shell built on top of bash. [Oh My Zsh](https://ohmyz.sh/) is a community-driven framework for managing zsh configuration.

### Installation

**Mac:**

```shell
brew install zsh
```

**Ubuntu:**

```shell
sudo apt -y install zsh
```

**Oh My Zsh:**

```shell
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
chsh -s $(which zsh)
```

### Configuration

See the [Oh My Zsh Wiki](https://github.com/ohmyzsh/ohmyzsh/wiki) for detailed information.

#### Themes

Change theme in `~/.zshrc`:

```shell
ZSH_THEME="agnoster"
```

Install [Powerlevel10k](https://github.com/romkatv/powerlevel10k):

```shell
git clone https://github.com/romkatv/powerlevel10k.git ~/.oh-my-zsh/custom/themes/powerlevel10k
```

```shell
ZSH_THEME="powerlevel10k/powerlevel10k"
```

#### Plugins

Standard plugins: `~/.oh-my-zsh/plugins/`
Custom plugins: `~/.oh-my-zsh/custom/plugins/`

**Install tools:**

**Mac:**

```shell
brew install fzf autojump
```

**Ubuntu:**

```shell
sudo apt -y install fzf autojump
```

**Install zsh plugins:**

```shell
git clone https://github.com/zsh-users/zsh-autosuggestions.git ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
git clone https://github.com/zsh-users/zsh-completions ~/.oh-my-zsh/custom/plugins/zsh-completions
```

**Configure in `~/.zshrc`:**

```shell
plugins=(
    brew
    git
    fzf
    autojump
    zsh-completions
    zsh-autosuggestions
    zsh-syntax-highlighting
)
```

#### Others

**Mac only - Disable homebrew auto update:**

```shell
export HOMEBREW_NO_AUTO_UPDATE="true"
```

---

## GnuPG

GnuPG (GPG) is a complete implementation of the OpenPGP standard for encrypting and signing data.

### Installation

**Mac:**

```shell
brew install gnupg
```

**Ubuntu:**

```shell
sudo apt -y install gnupg
```

### Configuration

Generate a new GPG key:

```shell
gpg --full-generate-key
```

---

## Pass

[Pass](https://www.passwordstore.org/) is a simple password manager following Unix philosophy.

### Installation

**Mac:**

```shell
brew install pass
```

**Ubuntu:**

```shell
sudo apt -y install pass
```

### Configuration

Initialize with a GPG key:

```shell
gpg --list-keys
pass init "your-gpg-id"
```

---

## LLVM

The LLVM Project provides modular compiler and toolchain technologies.

### Installation

**Mac:**

```shell
brew install llvm
```

**Ubuntu:**

```shell
sudo apt -y install llvm
```

### Configuration

**Mac only - Add to `~/.zshrc`:**

```shell
echo 'export PATH="$(brew --prefix llvm)/bin:$PATH"' >> ~/.zshrc
```

> **Note:** LLVM is also listed as an optional dependency in `emacs/setup.sh` for C/C++ LSP support.

---

## Golang

[Go](https://golang.org/) is an open source programming language.

### Installation

Download from [golang.org/dl](https://golang.org/dl/) and install manually.

### Configuration

Create workspace:

```shell
mkdir -p $HOME/MyGoProjects
```

Add to `~/.zshrc`:

```shell
# Path to your go projects
export GOPATH=$HOME/MyGoProjects
export PATH=$GOPATH/bin:$PATH
```

Install tools:

```shell
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/tools/gopls@latest
```

> **Note:** `gopls` and `gofumpt` are also listed in `emacs/setup.sh` for LSP support.

---

## Rust

[Rust](https://www.rust-lang.org/) is a systems programming language focused on safety and performance.

### Installation

Install via [rustup](https://www.rust-lang.org/tools/install).

### Configuration

**Install rust-analyzer:**

**Mac:**

```shell
brew install rust-analyzer
```

**Ubuntu:**

```shell
sudo apt -y install rust-analyzer
```

**Install rust source:**

```shell
rustup component add rust-src
```

---

## Pyenv

[pyenv](https://github.com/pyenv/pyenv) lets you easily switch between multiple Python versions.

### Installation

```shell
curl https://pyenv.run | bash
```

### Configuration

Add to `~/.zshrc`:

```shell
# Initialize pyenv
eval "$(pyenv init --path)"
eval "$(pyenv init -)"
```

---

## pyenv-virtualenvwrapper

[pyenv-virtualenvwrapper](https://github.com/pyenv/pyenv-virtualenvwrapper) provides virtualenvwrapper integration with pyenv.

### Installation

```shell
git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git $(pyenv root)/plugins/pyenv-virtualenvwrapper
```

### Configuration

Add to `~/.zshrc`:

```shell
export PYENV_VIRTUALENVWRAPPER_PREFER_PYVENV="true"
pyenv virtualenvwrapper
```

Create `~/.virtualenvs/postmkvirtualenv` for auto-installing tools:

```shell
pip install "python-lsp-server[all]"
pip install debugpy
pip install black black-macchiato isort
```

---

## Python

macOS ships with Python, but you should install your own version to avoid affecting system tools.

### Installation

Using pyenv:

```shell
pyenv install 3.13.1
pyenv global 3.13.1
pyenv rehash
```

> **Note:** Python LSP and debug tools are listed in `emacs/setup.sh` for Emacs integration.

---

## Emacs

[Emacs](https://www.gnu.org/software/emacs/) is an extensible, customizable text editor.

### Installation

**Mac:**

```shell
brew install --cask emacs-app
```

**Ubuntu:**

```shell
sudo snap install emacs --classic
```

### Configuration

Run the setup script:

```shell
./emacs/setup.sh
```

The setup script will:
- Create symbolic link from `~/.emacs` to the project configuration
- Display a comprehensive dependency installation guide including:
  - Core tools (git, aspell, pandoc, ripgrep, etc.)
  - LSP servers (Python, Go, C/C++, Haskell, YAML, Bash, Dockerfile, CMake, Markdown)
  - Code formatters and linters
  - Debug tools
  - Fonts for GUI mode

After setup, install fonts in Emacs:

```text
M-x nerd-icons-install-fonts
```

---

## Vim

[Vim](https://www.vim.org/) is a highly configurable text editor.

### Installation

**Mac:**

```shell
brew install vim
```

**Ubuntu:**

```shell
sudo apt -y install vim
```

### Configuration

Run the setup script:

```shell
./vim/setup.sh
```

The setup script will:
- Create symbolic link from `~/.vimrc` to the project configuration
- Create necessary directories (`~/.vim/undo`, `~/.vim/backup`, `~/.vim/swap`)
- Display optional tools for enhanced experience (ripgrep, fd, vim-plug)

**Neovim users:**

```shell
mkdir -p ~/.config/nvim
ln -s ~/.vimrc ~/.config/nvim/init.vim
```
