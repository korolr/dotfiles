r"""
Backrefs re.

Add the ability to use the following backrefs with re:

 - `\l`                                                          - Lowercase character class (search)
 - `\c`                                                          - Uppercase character class (search)
 - `\L`                                                          - Inverse of lowercase character class (search)
 - `\C`                                                          - Inverse of uppercase character class (search)
 - `\Q` and `\Q...\E`                                            - Escape/quote chars (search)
 - `\c` and `\C...\E`                                            - Uppercase char or chars (replace)
 - `\l` and `\L...\E`                                            - Lowercase char or chars (replace)
 - `[:ascii:]`                                                   - Posix style classes (search)
 - `[:^ascii:]`                                                  - Inverse Posix style classes (search)
 - `\pL`, `\p{Lu}`, \p{Letter}, `\p{gc=Uppercase_Letter}`        - Unicode properties (search Unicode)
 - `\PL`, `\P{Lu}`, `\p{^Lu}`                                    - Inverse Unicode properties (search Unicode)
 - `\N{Black Club Suit}`                                         - Unicode character by name (search & replace)
 - `\u0000` and `\U00000000`                                     - Unicode characters (replace)
 - `\e`                                                          - Escape character (search)
 - `\m`                                                          - Starting word boundary (search)
 - `\M`                                                          - Ending word boundary (search)
 - `\R`                                                          - Generic line breaks (search)
 - `\X`                                                          - Simplified grapheme clusters (search)

Licensed under MIT
Copyright (c) 2011 - 2018 Isaac Muse <isaacmuse@gmail.com>
"""
from __future__ import unicode_literals
import sys as _sys
import re as _re
from . import util as _util
from . import _bre_parse
from ._bre_parse import ReplaceTemplate

__all__ = (
    "expand", "expandf", "search", "match", "fullmatch", "split", "findall", "finditer", "sub", "subf",
    "subn", "subfn", "purge", "escape", "DEBUG", "I", "IGNORECASE", "L", "LOCALE", "M", "MULTILINE",
    "S", "DOTALL", "U", "UNICODE", "X", "VERBOSE", "compile", "compile_search", "compile_replace", "Bregex",
    "ReplaceTemplate"
) + (("A", "ASCII") if _util.PY3 else tuple()) + (("fullmatch",) if _util.PY34 else tuple())

# Expose some common re flags and methods to
# save having to import re and backrefs libs
DEBUG = _re.DEBUG
I = _re.I
IGNORECASE = _re.IGNORECASE
L = _re.L
LOCALE = _re.LOCALE
M = _re.M
MULTILINE = _re.MULTILINE
S = _re.S
DOTALL = _re.DOTALL
U = _re.U
UNICODE = _re.UNICODE
X = _re.X
VERBOSE = _re.VERBOSE
if _util.PY3:
    A = _re.A
    ASCII = _re.ASCII
escape = _re.escape

# Replace flags
FORMAT = 1

# Maximum size of the cache.
_MAXCACHE = 500

_RE_TYPE = type(_re.compile('', 0))


@_util.lru_cache(maxsize=_MAXCACHE)
def _cached_search_compile(pattern, re_verbose, re_version, pattern_type):
    """Cached search compile."""

    return _bre_parse._SearchParser(pattern, re_verbose, re_version).parse()


@_util.lru_cache(maxsize=_MAXCACHE)
def _cached_replace_compile(pattern, repl, flags, pattern_type):
    """Cached replace compile."""

    return _bre_parse._ReplaceParser().parse(pattern, repl, bool(flags & FORMAT))


def _get_cache_size(replace=False):
    """Get size of cache."""

    if not replace:
        size = _cached_search_compile.cache_info().currsize
    else:
        size = _cached_replace_compile.cache_info().currsize
    return size


def _purge_cache():
    """Purge the cache."""

    _cached_replace_compile.cache_clear()
    _cached_search_compile.cache_clear()


def _is_replace(obj):
    """Check if object is a replace object."""

    return isinstance(obj, ReplaceTemplate)


def _apply_replace_backrefs(m, repl=None, flags=0):
    """Expand with either the `ReplaceTemplate` or compile on the fly, or return None."""

    if m is None:
        raise ValueError("Match is None!")
    else:
        if isinstance(repl, ReplaceTemplate):
            return repl.expand(m)
        elif isinstance(repl, (_util.string_type, _util.binary_type)):
            return _bre_parse._ReplaceParser().parse(m.re, repl, bool(flags & FORMAT)).expand(m)


def _apply_search_backrefs(pattern, flags=0):
    """Apply the search backrefs to the search pattern."""

    if isinstance(pattern, (_util.string_type, _util.binary_type)):
        re_verbose = bool(VERBOSE & flags)
        re_unicode = None
        if _util.PY3 and bool((ASCII | LOCALE) & flags):
            re_unicode = False
        elif bool(UNICODE & flags):
            re_unicode = True
        if not (flags & DEBUG):
            pattern = _cached_search_compile(pattern, re_verbose, re_unicode, type(pattern))
        else:  # pragma: no cover
            pattern = _bre_parse._SearchParser(pattern, re_verbose, re_unicode).parse()
    elif isinstance(pattern, Bre):
        if flags:
            raise ValueError("Cannot process flags argument with a compiled pattern")
        pattern = pattern._pattern
    elif isinstance(pattern, (_RE_TYPE, Bre)):
        if flags:
            raise ValueError("Cannot process flags argument with a compiled pattern!")
    else:
        raise TypeError("Not a string or compiled pattern!")
    return pattern


def _assert_expandable(repl, use_format=False):
    """Check if replace template is expandable."""

    if isinstance(repl, ReplaceTemplate):
        if repl.use_format != use_format:
            if use_format:
                raise ValueError("Replace not compiled as a format replace")
            else:
                raise ValueError("Replace should not be compiled as a format replace!")
    elif not isinstance(repl, (_util.string_type, _util.binary_type)):
        raise TypeError("Expected string, buffer, or compiled replace!")


###########################
# API
##########################
class Bre(_util.Immutable):
    """Bre object."""

    __slots__ = ("_pattern", "auto_compile", "_hash")

    def __init__(self, pattern, auto_compile=True):
        """Initialization."""

        super(Bre, self).__init__(
            _pattern=pattern,
            auto_compile=auto_compile,
            _hash=hash((type(self), type(pattern), pattern, auto_compile))
        )

    @property
    def pattern(self):
        """Return pattern."""

        return self._pattern.pattern

    @property
    def flags(self):
        """Return flags."""

        return self._pattern.flags

    @property
    def groupindex(self):
        """Return group index."""

        return self._pattern.groupindex

    @property
    def groups(self):
        """Return groups."""

        return self._pattern.groups

    @property
    def scanner(self):
        """Return scanner."""

        return self._pattern.scanner

    def __hash__(self):
        """Hash."""

        return self._hash

    def __eq__(self, other):
        """Equal."""

        return (
            isinstance(other, Bre) and
            self._pattern == other._pattern and
            self.auto_compile == other.auto_compile
        )

    def __ne__(self, other):
        """Equal."""

        return (
            not isinstance(other, Bre) or
            self._pattern != other._pattern or
            self.auto_compile != other.auto_compile
        )

    def __repr__(self):  # pragma: no cover
        """Representation."""

        return '%s.%s(%r, auto_compile=%r)' % (
            self.__module__, self.__class__.__name__, self._pattern, self.auto_compile
        )

    def _auto_compile(self, template, use_format=False):
        """Compile replacements."""

        is_replace = _is_replace(template)
        is_string = isinstance(template, (_util.string_type, _util.binary_type))
        if is_replace and use_format != template.use_format:
            raise ValueError("Compiled replace cannot be a format object!")
        if is_replace or (is_string and self.auto_compile):
            return self.compile(template, (FORMAT if use_format and not is_replace else 0))
        elif is_string and use_format:
            # Reject an attempt to run format replace when auto-compiling
            # of template strings has been disabled and we are using a
            # template string.
            raise AttributeError('Format replaces cannot be called without compiling replace template!')
        else:
            return template

    def compile(self, repl, flags=0):
        """Compile replace."""

        return compile_replace(self._pattern, repl, flags)

    def search(self, string, pos=0, endpos=_sys.maxsize):
        """Apply `search`."""

        return self._pattern.search(string, pos, endpos)

    def match(self, string, pos=0, endpos=_sys.maxsize):
        """Apply `match`."""

        return self._pattern.match(string, pos, endpos)

    if _util.PY34:
        def fullmatch(self, string, pos=0, endpos=_sys.maxsize):
            """Apply `fullmatch`."""

            return self._pattern.fullmatch(string, pos, endpos)

    def split(self, string, maxsplit=0):
        """Apply `split`."""

        return self._pattern.split(string, maxsplit)

    def findall(self, string, pos=0, endpos=_sys.maxsize):
        """Apply `findall`."""

        return self._pattern.findall(string, pos, endpos)

    def finditer(self, string, pos=0, endpos=_sys.maxsize):
        """Apply `finditer`."""

        return self._pattern.finditer(string, pos, endpos)

    def sub(self, repl, string, count=0):
        """Apply `sub`."""

        return self._pattern.sub(self._auto_compile(repl), string, count)

    def subf(self, repl, string, count=0):  # noqa A002
        """Apply `sub` with format style replace."""

        return self._pattern.sub(self._auto_compile(repl, True), string, count)

    def subn(self, repl, string, count=0):
        """Apply `subn` with format style replace."""

        return self._pattern.subn(self._auto_compile(repl), string, count)

    def subfn(self, repl, string, count=0):  # noqa A002
        """Apply `subn` after applying backrefs."""

        return self._pattern.subn(self._auto_compile(repl, True), string, count)


def compile(pattern, flags=0, auto_compile=None):
    """Compile both the search or search and replace into one object."""

    if isinstance(pattern, Bre):
        if auto_compile is not None:
            raise ValueError("Cannot compile Bre with a different auto_compile!")
        elif flags != 0:
            raise ValueError("Cannot process flags argument with a compiled pattern")
        return pattern
    else:
        if auto_compile is None:
            auto_compile = True

        return Bre(compile_search(pattern, flags), auto_compile)


def compile_search(pattern, flags=0):
    """Compile with extended search references."""

    return _re.compile(_apply_search_backrefs(pattern, flags), flags)


def compile_replace(pattern, repl, flags=0):
    """Construct a method that can be used as a replace method for `sub`, `subn`, etc."""

    call = None
    if pattern is not None and isinstance(pattern, _RE_TYPE):
        if isinstance(repl, (_util.string_type, _util.binary_type)):
            if not (pattern.flags & DEBUG):
                call = _cached_replace_compile(pattern, repl, flags, type(repl))
            else:  # pragma: no cover
                call = _bre_parse._ReplaceParser().parse(pattern, repl, bool(flags & FORMAT))
        elif isinstance(repl, ReplaceTemplate):
            if flags:
                raise ValueError("Cannot process flags argument with a ReplaceTemplate!")
            if repl.pattern_hash != hash(pattern):
                raise ValueError("Pattern hash doesn't match hash in compiled replace!")
            call = repl
        else:
            raise TypeError("Not a valid type!")
    else:
        raise TypeError("Pattern must be a compiled regular expression!")
    return call


def purge():
    """Purge caches."""

    _purge_cache()
    _re.purge()


def expand(m, repl):
    """Expand the string using the replace pattern or function."""

    _assert_expandable(repl)
    return _apply_replace_backrefs(m, repl)


def expandf(m, format):  # noqa A002
    """Expand the string using the format replace pattern or function."""

    _assert_expandable(format, True)
    return _apply_replace_backrefs(m, format, flags=FORMAT)


def search(pattern, string, flags=0):
    """Apply `search` after applying backrefs."""

    return _re.search(_apply_search_backrefs(pattern, flags), string, flags)


def match(pattern, string, flags=0):
    """Apply `match` after applying backrefs."""

    return _re.match(_apply_search_backrefs(pattern, flags), string, flags)


if _util.PY34:
    def fullmatch(pattern, string, flags=0):
        """Apply `fullmatch` after applying backrefs."""

        return _re.fullmatch(_apply_search_backrefs(pattern, flags), string, flags)


def split(pattern, string, maxsplit=0, flags=0):
    """Apply `split` after applying backrefs."""

    return _re.split(_apply_search_backrefs(pattern, flags), string, maxsplit, flags)


def findall(pattern, string, flags=0):
    """Apply `findall` after applying backrefs."""

    return _re.findall(_apply_search_backrefs(pattern, flags), string, flags)


def finditer(pattern, string, flags=0):
    """Apply `finditer` after applying backrefs."""

    return _re.finditer(_apply_search_backrefs(pattern, flags), string, flags)


def sub(pattern, repl, string, count=0, flags=0):
    """Apply `sub` after applying backrefs."""

    is_replace = _is_replace(repl)
    is_string = isinstance(repl, (_util.string_type, _util.binary_type))
    if is_replace and repl.use_format:
        raise ValueError("Compiled replace cannot be a format object!")

    pattern = compile_search(pattern, flags)
    return _re.sub(
        pattern, (compile_replace(pattern, repl) if is_replace or is_string else repl), string, count, flags
    )


def subf(pattern, format, string, count=0, flags=0):  # noqa A002
    """Apply `sub` with format style replace."""

    is_replace = _is_replace(format)
    is_string = isinstance(format, (_util.string_type, _util.binary_type))
    if is_replace and not format.use_format:
        raise ValueError("Compiled replace is not a format object!")

    pattern = compile_search(pattern, flags)
    rflags = FORMAT if is_string else 0
    return _re.sub(
        pattern, (compile_replace(pattern, format, flags=rflags) if is_replace or is_string else format),
        string, count, flags
    )


def subn(pattern, repl, string, count=0, flags=0):
    """Apply `subn` with format style replace."""

    is_replace = _is_replace(repl)
    is_string = isinstance(repl, (_util.string_type, _util.binary_type))
    if is_replace and repl.use_format:
        raise ValueError("Compiled replace cannot be a format object!")

    pattern = compile_search(pattern, flags)
    return _re.subn(
        pattern, (compile_replace(pattern, repl) if is_replace or is_string else repl), string, count, flags
    )


def subfn(pattern, format, string, count=0, flags=0):  # noqa A002
    """Apply `subn` after applying backrefs."""

    is_replace = _is_replace(format)
    is_string = isinstance(format, (_util.string_type, _util.binary_type))
    if is_replace and not format.use_format:
        raise ValueError("Compiled replace is not a format object!")

    pattern = compile_search(pattern, flags)
    rflags = FORMAT if is_string else 0
    return _re.subn(
        pattern, (compile_replace(pattern, format, flags=rflags) if is_replace or is_string else format),
        string, count, flags
    )


def _pickle(p):
    return Bre, (p._pattern, p.auto_compile)


_util.copyreg.pickle(Bre, _pickle)
