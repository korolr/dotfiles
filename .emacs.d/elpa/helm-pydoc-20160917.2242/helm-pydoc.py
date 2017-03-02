#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
from os.path import basename, splitext
import pkgutil

myname = (splitext(basename(sys.argv[0])))[0]
modules = [m[1] for m in pkgutil.iter_modules() if m[1] != myname]
modules.append('sys')  # builtin module
modules.sort()

for module in modules:
    print(module)
