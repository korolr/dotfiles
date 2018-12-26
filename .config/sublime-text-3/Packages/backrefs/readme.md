# Backrefs

Current Version: 3.5.0

Wraps the Python `re` or `regex` module to provide additional back references.  On Sublime, currently only `re` is available.  But if your system has some how made `regex` available, it should work for `regex` as well.

## Overview

Backrefs was written to add various additional back references that are known to some regex engines, but not to Python's re or regex.  The back references that are added differ depending on the regular expression engine being used as some back references may already be supported.

To learn more about what backrefs can do, read the [official documentation](http://facelessuser.github.io/backrefs/).
