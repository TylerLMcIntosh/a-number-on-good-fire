from pathlib import Path
import sys

def find_set_project_root(markers=(".git", "pyproject.toml", ".here")) -> Path:
    """
    Find the project root and add it to sys.path for relative imports.

    Parameters:
    - markers: Tuple of files/folders that indicate the root.

    Returns:
    - Path to the project root.

    Raises:
    - FileNotFoundError if no marker is found.
    """
    try:
        base = Path(__file__).resolve().parent
    except NameError:
        base = Path.cwd()

    for parent in [base] + list(base.parents):
        if any((parent / marker).exists() for marker in markers):
            root_path = str(parent)
            if root_path not in sys.path:
                sys.path.insert(0, root_path)  # Prepend for import priority
            return parent

    raise FileNotFoundError(f"❌ No marker found: {markers}")