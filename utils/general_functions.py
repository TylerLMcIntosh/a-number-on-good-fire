

from pathlib import Path
from typing import Union, List

def dir_ensure(paths: Union[str, List[str]]) -> List[Path]:
    """
    Ensure that one or more directories exist, creating them if necessary.

    Parameters:
    ----------
    paths : str or list of str or Path
        A single directory path or a list of directory paths to check and create if missing.

    Returns:
    -------
    List[Path]
        A list of Path objects corresponding to the input paths.

    Raises:
    ------
    TypeError
        If the input is not a string, Path, or list/tuple of those types.

    Side Effects:
    -------------
    - Creates directories on the file system if they do not exist.
    - Prints messages to stdout indicating status for each path.

    Examples:
    --------
    >>> dir_ensure("output")
    [PosixPath('/absolute/path/to/output')]

    >>> dir_ensure(["data", "results"])
    [PosixPath('/abs/path/data'), PosixPath('/abs/path/results')]

    Requires:
    from pathlib import Path
    from typing import Union, List

    """
    if isinstance(paths, (str, Path)):
        paths = [paths]
    elif not isinstance(paths, (list, tuple)):
        raise TypeError("`paths` must be a string, Path, or list of strings/Paths.")

    created_paths = []

    for p in paths:
        path = Path(p).expanduser().resolve()
        try:
            if not path.exists():
                path.mkdir(parents=True, exist_ok=True)
                print(f"📁 Directory created: {path}")
            else:
                print(f"✅ Directory already exists: {path}")
            created_paths.append(path)
        except Exception as e:
            print(f"⚠️ Failed to create directory: {path} — {e}")
    return created_paths
