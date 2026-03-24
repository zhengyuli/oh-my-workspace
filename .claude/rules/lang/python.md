# Python Conventions

Python coding standards for oh-my-workspace.

## References

- [Google Python Style Guide](https://google.github.io/styleguide/pyguide.html) - Primary reference
- [PEP 8](https://peps.python.org/pep-0008/) - Base standard

## Code Style

### Line Length

- **Maximum:** 80 characters
- **Docstrings:** 72 characters for content

### Indentation

- **Spaces:** 4 spaces (no tabs)
- **Continuation:** Align with opening delimiter or 4-space indent

```python
# Good - align with opening delimiter
result = some_function(
    arg1, arg2,
    arg3, arg4)

# Good - 4-space indent
def long_function_name(
        var_one, var_two, var_three,
        var_four):
    ...
```

### Naming

| Type | Convention | Example |
|------|------------|---------|
| Module | snake_case | `dotfiles_manager.py` |
| Class | PascalCase | `DotfilesManager` |
| Function | snake_case | `install_package` |
| Variable | snake_case | `package_name` |
| Constant | UPPER_SNAKE_CASE | `DEFAULT_TIMEOUT` |
| Private | _leading_underscore | `_internal_func` |

### Imports

Order: stdlib → third-party → local

```python
# Standard library
import os
import sys
from pathlib import Path

# Third-party
import click
from rich.console import Console

# Local
from .utils import symlink
from .config import Config
```

## Type Hints

### Always Use Type Hints

```python
from typing import Optional, List

def install_packages(
    packages: List[str],
    dry_run: bool = False,
) -> bool:
    """Install packages using stow."""
    ...

def get_config(key: str) -> Optional[str]:
    """Get config value or None if not found."""
    ...
```

### Modern Syntax (Python 3.9+)

```python
# Good - modern
def process(items: list[str]) -> dict[str, int]:
    ...

# Avoid - old style
from typing import List, Dict
def process(items: List[str]) -> Dict[str, int]:
    ...
```

### Union Types (Python 3.10+)

```python
# Good - modern
def get_value(key: str) -> str | None:
    ...

# Avoid - old style
from typing import Optional
def get_value(key: str) -> Optional[str]:
    ...
```

## Documentation

### Docstrings

Use Google style docstrings:

```python
def install_package(name: str, force: bool = False) -> bool:
    """Install a dotfiles package using GNU Stow.

    Creates symlinks in the user's home directory for the
    specified package. Handles conflicts according to the
    force parameter.

    Args:
        name: Package name (e.g., "shell/zsh").
        force: If True, overwrite existing files without prompting.

    Returns:
        True if installation succeeded, False otherwise.

    Raises:
        ValueError: If package name is invalid.
        FileNotFoundError: If package directory doesn't exist.

    Example:
        >>> install_package("shell/zsh")
        True
    """
    ...
```

### Module Docstrings

```python
"""Package management for oh-my-workspace dotfiles.

This module provides utilities for installing, updating, and
managing dotfiles packages using GNU Stow.

Example usage:
    from dotfiles import install_packages
    install_packages(["shell/zsh", "editor/emacs"])
"""
```

## Error Handling

### Specific Exceptions

```python
# Good - specific exception
try:
    config = load_config(path)
except FileNotFoundError:
    print(f"Config not found: {path}")
    return None
except json.JSONDecodeError as e:
    print(f"Invalid JSON in config: {e}")
    return None

# Bad - bare except
try:
    config = load_config(path)
except:
    print("Something went wrong")
    return None
```

### Context Managers

```python
# Good - automatic cleanup
with open(config_file) as f:
    config = json.load(f)

# Also good - custom context manager
from contextlib import contextmanager

@contextmanager
def stow_context(package: str):
    """Context manager for stow operations."""
    prepare_stow(package)
    try:
        yield
    finally:
        cleanup_stow(package)
```

### Raise Meaningful Exceptions

```python
def validate_package(name: str) -> None:
    """Validate package name.

    Raises:
        ValueError: If name is empty or contains invalid characters.
    """
    if not name:
        raise ValueError("Package name cannot be empty")
    if not re.match(r'^[a-z0-9/-]+$', name):
        raise ValueError(f"Invalid package name: {name}")
```

## Dependencies

### Use uv

This project uses `uv` for package management:

```bash
# Add dependency
uv add requests

# Add dev dependency
uv add --dev pytest

# Install from requirements
uv sync
```

### pyproject.toml

Declare dependencies in `pyproject.toml`:

```toml
[project]
name = "oh-my-workspace"
version = "1.0.0"
requires-python = ">=3.10"
dependencies = [
    "click>=8.0",
    "rich>=13.0",
]

[project.optional-dependencies]
dev = [
    "pytest>=7.0",
    "black>=23.0",
    "ruff>=0.1.0",
]
```

## Testing

### pytest

Use pytest for tests:

```python
# tests/test_install.py
import pytest
from dotfiles import install_package

def test_install_valid_package():
    """Test installing a valid package."""
    result = install_package("shell/zsh")
    assert result is True

def test_install_invalid_package():
    """Test installing an invalid package raises error."""
    with pytest.raises(ValueError):
        install_package("invalid/package")

@pytest.fixture
def temp_home(tmp_path):
    """Create a temporary home directory."""
    home = tmp_path / "home"
    home.mkdir()
    return home
```

### Run Tests

```bash
# Run all tests
uv run pytest

# Run specific test
uv run pytest tests/test_install.py::test_install_valid_package

# Run with coverage
uv run pytest --cov=dotfiles
```