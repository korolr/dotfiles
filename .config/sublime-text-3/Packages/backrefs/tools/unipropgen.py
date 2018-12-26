"""
Generate a Unicode prop table for Python narrow and wide builds.

Narrow builds will stop at `0xffff`.
"""
from __future__ import unicode_literals
import sys
import struct
import unicodedata
import codecs
import os
import re

__version__ = '4.2.0'

UNIVERSION = unicodedata.unidata_version
UNIVERSION_INFO = tuple([int(x) for x in UNIVERSION.split('.')])
HOME = os.path.dirname(os.path.abspath(__file__))
MAXUNICODE = sys.maxunicode
MAXASCII = 0xFF
NARROW = sys.maxunicode == 0xFFFF
GROUP_ESCAPES = frozenset([ord(x) for x in '-&[\\]^|~'])

# Compatibility
PY3 = sys.version_info >= (3, 0) and sys.version_info[0:2] < (4, 0)
PY34 = sys.version_info >= (3, 4)
PY35 = sys.version_info >= (3, 5)
PY37 = sys.version_info >= (3, 7)

if NARROW:
    UNICODE_RANGE = (0x0000, 0xFFFF)
else:
    UNICODE_RANGE = (0x0000, 0x10FFFF)
ASCII_RANGE = (0x00, 0xFF)
if PY3:
    unichar = chr  # noqa
else:
    unichar = unichr  # noqa

ALL_CHARS = set([x for x in range(UNICODE_RANGE[0], UNICODE_RANGE[1] + 1)])
ALL_ASCII = set([x for x in range(ASCII_RANGE[0], ASCII_RANGE[1] + 1)])
HEADER = '''\
"""Unicode Properties from Unicode version %s (autogen)."""
from __future__ import unicode_literals

''' % UNIVERSION


def uchr(i):
    """Allow getting Unicode character on narrow Python builds."""

    try:
        return unichar(i)
    except ValueError:
        return struct.pack('i', i).decode('utf-32')


def uniformat(value):
    """Convert a Unicode char."""

    if value in GROUP_ESCAPES:
        # Escape characters that are (or will be in the future) problematic
        c = "\\x%02x\\x%02x" % (0x5c, value)
    elif value <= 0xFF:
        c = "\\x%02x" % value
    elif value <= 0xFFFF:
        c = "\\u%04x" % value
    else:
        c = "\\U%08x" % value
    return c


def format_name(text):
    """Format the name."""
    return text.strip().lower().replace(' ', '').replace('-', '').replace('_', '')


def binaryformat(value):
    """Convert a binary value."""

    if value in GROUP_ESCAPES:
        # Escape characters that are (or will be in the future) problematic
        c = "\\x%02x\\x%02x" % (0x5c, value)
    else:
        c = "\\x%02x" % value
    return c


def create_span(unirange, binary=False):
    """Clamp the Unicode range."""

    if len(unirange) < 2:
        unirange.append(unirange[0])
    if binary:
        if unirange[0] > MAXASCII:
            return None
        if unirange[1] > MAXASCII:
            unirange[1] = MAXASCII
    elif NARROW:
        if unirange[0] > MAXUNICODE:
            return None
        if unirange[1] > MAXUNICODE:
            unirange[1] = MAXUNICODE
    return [x for x in range(unirange[0], unirange[1] + 1)]


def not_explicitly_defined(table, name, binary=False):
    """Compose a table with the specified entry name of values not explicitly defined."""

    all_chars = ALL_ASCII if binary else ALL_CHARS
    s = set()
    for k, v in table.items():
        s.update(v)
    if name in table:
        table[name] = list(set(table[name]) | (all_chars - s))
    else:
        table[name] = list(all_chars - s)


def char2range(d, binary=False, invert=True):
    """Convert the characters in the dict to a range in string form."""

    fmt = binaryformat if binary else uniformat
    maxrange = MAXASCII if binary else MAXUNICODE

    for k1 in sorted(d.keys()):
        v1 = d[k1]
        if not isinstance(v1, list):
            char2range(v1, binary=binary, invert=invert)
        else:
            inverted = k1.startswith('^')
            v1.sort()
            last = None
            first = None
            ilast = None
            ifirst = None
            v2 = []
            iv2 = []
            if v1 and v1[0] != 0:
                ifirst = 0
            for i in v1:
                if first is None:
                    first = i
                    last = i
                elif i == last + 1:
                    last = i
                elif first is not None:
                    if first == last:
                        v2.append(fmt(first))
                    else:
                        v2.append("%s-%s" % (fmt(first), fmt(last)))
                    if invert and ifirst is not None:
                        ilast = first - 1
                        if ifirst == ilast:
                            iv2.append(fmt(ifirst))
                        else:
                            iv2.append("%s-%s" % (fmt(ifirst), fmt(ilast)))
                    ifirst = last + 1
                    first = i
                    last = i

            if not v1:
                iv2 = ["%s-%s" % (fmt(0), fmt(maxrange))]
            elif first is not None:
                if first == last:
                    v2.append(fmt(first))
                else:
                    v2.append("%s-%s" % (fmt(first), fmt(last)))
                if invert and ifirst is not None:
                    ilast = first - 1
                    if ifirst == ilast:
                        iv2.append(fmt(ifirst))
                    else:
                        iv2.append("%s-%s" % (fmt(ifirst), fmt(ilast)))
                ifirst = last + 1
                if invert and ifirst <= maxrange:
                    ilast = maxrange
                    if ifirst == ilast:
                        iv2.append(fmt(ifirst))
                    else:
                        iv2.append("%s-%s" % (fmt(ifirst), fmt(ilast)))
            d[k1] = ''.join(v2)
            if invert:
                d[k1[1:] if inverted else '^' + k1] = ''.join(iv2)


def gen_blocks(output, ascii_props=False, append=False, prefix=""):
    """Generate Unicode blocks."""

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        f.write('%s_blocks = {' % prefix)
        no_block = []
        last = -1

        max_range = MAXASCII if ascii_props else MAXUNICODE
        formatter = binaryformat if ascii_props else uniformat

        with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'Blocks.txt'), 'r') as uf:
            for line in uf:
                if not line.startswith('#'):
                    data = line.split(';')
                    if len(data) < 2:
                        continue
                    block = [int(i, 16) for i in data[0].strip().split('..')]
                    if block[0] > last + 1:
                        if (last + 1) <= max_range:
                            endval = block[0] - 1 if (block[0] - 1) < max_range else max_range
                            no_block.append((last + 1, endval))
                    last = block[1]
                    name = format_name(data[1])
                    inverse_range = []
                    if block[0] > max_range:
                        if ascii_props:
                            f.write('\n    "%s": "",' % name)
                            f.write('\n    "^%s": "%s-%s",' % (name, formatter(0), formatter(max_range)))
                        continue
                    if block[0] > 0:
                        inverse_range.append("%s-%s" % (formatter(0), formatter(block[0] - 1)))
                    if block[1] < max_range:
                        inverse_range.append("%s-%s" % (formatter(block[1] + 1), formatter(max_range)))
                    f.write('\n    "%s": "%s-%s",' % (name, formatter(block[0]), formatter(block[1])))
                    f.write('\n    "^%s": "%s",' % (name, ''.join(inverse_range)))
            if last < max_range:
                if (last + 1) <= max_range:
                    no_block.append((last + 1, max_range))
            last = -1
            no_block_inverse = []
            if not no_block:
                no_block_inverse.append((0, max_range))
            else:
                for piece in no_block:
                    if piece[0] > last + 1:
                        no_block_inverse.append((last + 1, piece[0] - 1))
                    last = piece[1]
            for block, name in ((no_block, 'noblock'), (no_block_inverse, '^noblock')):
                f.write('\n    "%s": "' % name)
                for piece in block:
                    if piece[0] == piece[1]:
                        f.write(formatter(piece[0]))
                    else:
                        f.write("%s-%s" % (formatter(piece[0]), formatter(piece[1])))
                f.write('",')
            f.write('\n}\n')


def gen_ccc(output, ascii_props=False, append=False, prefix=""):
    """Generate `canonical combining class` property."""

    obj = {}
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'DerivedCombiningClass.txt'), 'r') as uf:
        for line in uf:
            if not line.startswith('#'):
                data = line.split('#')[0].split(';')
                if len(data) < 2:
                    continue
                span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                if span is None:
                    continue
                name = format_name(data[1])

                if name not in obj:
                    obj[name] = []
                obj[name].extend(span)

    for x in range(0, 256):
        key = str(x)
        if key not in obj:
            obj[key] = []

    for name in list(obj.keys()):
        s = set(obj[name])
        obj[name] = sorted(s)

    not_explicitly_defined(obj, '0', binary=ascii_props)

    # Convert characters values to ranges
    char2range(obj, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_canonical_combining_class = {\n' % prefix)
        count = len(obj) - 1
        i = 0
        for k1, v1 in sorted(obj.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_scripts(
    file_name, file_name_ext, obj_name, obj_ext_name, output, output_ext,
    field=1, notexplicit=None, ascii_props=False, append=False, prefix=""
):
    """Generate `script` property."""

    obj = {}
    obj2 = {}
    if PY3:
        aliases = {}
        with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'PropertyValueAliases.txt'), 'r') as uf:
            for line in uf:
                if line.startswith('sc ;'):
                    values = line.split(';')
                    aliases[format_name(values[1].strip())] = format_name(values[2].strip())

        with open(os.path.join(HOME, 'unicodedata', UNIVERSION, file_name_ext), 'r') as uf:
            for line in uf:
                if not line.startswith('#'):
                    data = line.split('#')[0].split(';')
                    if len(data) < 2:
                        continue
                    exts = [aliases[format_name(n)] for n in data[1].strip().split(' ')]
                    span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                    for ext in exts:
                        if ext not in obj2:
                            obj2[ext] = []
                        if span is None:
                            continue

                        obj2[ext].extend(span)

    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, file_name), 'r') as uf:
        for line in uf:
            if not line.startswith('#'):
                data = line.split('#')[0].split(';')
                if len(data) < 2:
                    continue
                span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                name = format_name(data[1])
                if name not in obj:
                    obj[name] = []
                if name not in obj2:
                    obj2[name] = []

                if span is None:
                    continue

                obj[name].extend(span)
                obj2[name].extend(span)

    for name in list(obj.keys()):
        s = set(obj[name])
        obj[name] = sorted(s)

    for name in list(obj2.keys()):
        s = set(obj2[name])
        obj2[name] = sorted(s)

    if notexplicit:
        not_explicitly_defined(obj, notexplicit, binary=ascii_props)
        not_explicitly_defined(obj2, notexplicit, binary=ascii_props)

    # Convert characters values to ranges
    char2range(obj, binary=ascii_props)
    char2range(obj2, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_%s = {\n' % (prefix, obj_name))
        count = len(obj) - 1
        i = 0
        for k1, v1 in sorted(obj.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1

    if PY3:
        with codecs.open(output_ext, 'a' if append else 'w', 'utf-8') as f:
            if not append:
                f.write(HEADER)
            # Write out the unicode properties
            f.write('%s_%s = {\n' % (prefix, obj_ext_name))
            count = len(obj2) - 1
            i = 0
            for k1, v1 in sorted(obj2.items()):
                f.write('    "%s": "%s"' % (k1, v1))
                if i == count:
                    f.write('\n}\n')
                else:
                    f.write(',\n')
                i += 1


def gen_enum(file_name, obj_name, output, field=1, notexplicit=None, ascii_props=False, append=False, prefix=""):
    """Generate generic enum."""

    obj = {}
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, file_name), 'r') as uf:
        for line in uf:
            if not line.startswith('#'):
                data = line.split('#')[0].split(';')
                if len(data) < 2:
                    continue
                span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                name = format_name(data[field])
                if name not in obj:
                    obj[name] = []

                if span is None:
                    continue

                obj[name].extend(span)

    for name in list(obj.keys()):
        s = set(obj[name])
        obj[name] = sorted(s)

    if notexplicit:
        not_explicitly_defined(obj, notexplicit, binary=ascii_props)

    # Convert characters values to ranges
    char2range(obj, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_%s = {\n' % (prefix, obj_name))
        count = len(obj) - 1
        i = 0
        for k1, v1 in sorted(obj.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_age(output, ascii_props=False, append=False, prefix=""):
    """Generate `age` property."""

    obj = {}
    all_chars = ALL_ASCII if ascii_props else ALL_CHARS
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'DerivedAge.txt'), 'r') as uf:
        for line in uf:
            if not line.startswith('#'):
                data = line.split('#')[0].split(';')
                if len(data) < 2:
                    continue
                span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                name = format_name(data[1])

                if name not in obj:
                    obj[name] = []

                if span is None:
                    continue

                obj[name].extend(span)

    unassigned = set()
    for x in obj.values():
        unassigned |= set(x)
    obj['na'] = list(all_chars - unassigned)

    for name in list(obj.keys()):
        s = set(obj[name])
        obj[name] = sorted(s)

    # Convert characters values to ranges
    char2range(obj, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_age = {\n' % prefix)
        count = len(obj) - 1
        i = 0
        for k1, v1 in sorted(obj.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_nf_quick_check(output, ascii_props=False, append=False, prefix=""):
    """Generate quick check properties."""

    categories = []
    nf = {}
    all_chars = ALL_ASCII if ascii_props else ALL_CHARS
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'DerivedNormalizationProps.txt'), 'r') as uf:
        for line in uf:
            if not line.startswith('#'):
                data = line.split('#')[0].split(';')
                if len(data) < 2:
                    continue
                if not data[1].strip().lower().endswith('_qc'):
                    continue
                span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                if span is None:
                    continue
                name = format_name(data[1][:-3] + 'quickcheck')
                subvalue = format_name(data[2])

                if name not in nf:
                    nf[name] = {}
                    categories.append(name)
                if subvalue not in nf[name]:
                    nf[name][subvalue] = []
                nf[name][subvalue].extend(span)

    for k1, v1 in nf.items():
        temp = set()
        for k2 in list(v1.keys()):
            temp |= set(v1[k2])
        v1['y'] = list(all_chars - temp)

    for k1, v1 in nf.items():
        for name in list(v1.keys()):
            s = set(nf[k1][name])
            nf[k1][name] = sorted(s)

    # Convert characters values to ranges
    char2range(nf, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        for key, value in sorted(nf.items()):
            # Write out the unicode properties
            f.write('%s_%s = {\n' % (prefix, key.replace('quickcheck', '_quick_check')))
            count = len(value) - 1
            i = 0
            for k1, v1 in sorted(value.items()):
                f.write('    "%s": "%s"' % (k1, v1))
                if i == count:
                    f.write('\n}\n')
                else:
                    f.write(',\n')
                i += 1

    return categories


def gen_binary(table, output, ascii_props=False, append=False, prefix=""):
    """Generate binary properties."""

    categories = []
    binary_props = (
        ('DerivedCoreProperties.txt', None),
        ('PropList.txt', None),
        ('DerivedNormalizationProps.txt', ('Changes_When_NFKC_Casefolded', 'Full_Composition_Exclusion'))
    )
    binary = {}
    for filename, include in binary_props:
        with open(os.path.join(HOME, 'unicodedata', UNIVERSION, filename), 'r') as uf:
            for line in uf:
                if not line.startswith('#'):
                    data = line.split('#')[0].split(';')
                    if len(data) < 2:
                        continue
                    if include and data[1].strip() not in include:
                        continue
                    span = create_span([int(i, 16) for i in data[0].strip().split('..')], binary=ascii_props)
                    name = format_name(data[1])

                    if name not in binary:
                        binary[name] = []
                        categories.append(name)
                    if span is None:
                        continue
                    binary[name].extend(span)

    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'CompositionExclusions.txt'), 'r') as uf:
        name = 'compositionexclusion'
        for line in uf:
            if not line.startswith('#'):
                data = [x.strip() for x in line.split('#')[0] if x.strip()]
                if not data:
                    continue
                span = create_span([int(data[0], 16)], binary=ascii_props)
                if span is None:
                    continue

                if name not in binary:
                    binary[name] = []
                    categories.append(name)
                binary[name].extend(span)
                binary['full' + name].extend(span)

    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'UnicodeData.txt'), 'r') as uf:
        name = 'bidimirrored'
        for line in uf:
            data = line.strip().split(';')
            if data:
                if data[9].strip().lower() != 'y':
                    continue
                span = create_span([int(data[0].strip(), 16)], binary=ascii_props)
                if span is None:
                    continue

                if name not in binary:
                    binary[name] = []
                    categories.append(name)
                binary[name].extend(span)

    for name in list(binary.keys()):
        s = set(binary[name])
        binary[name] = sorted(s)

    gen_uposix(table, binary)

    # Convert characters values to ranges
    char2range(binary, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_binary = {\n' % prefix)
        count = len(binary) - 1
        i = 0
        for k1, v1 in sorted(binary.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1

    return categories[:]


def gen_bidi(output, ascii_props=False, append=False, prefix=""):
    """Generate `bidi class` property."""

    bidi_class = {}
    max_range = MAXASCII if ascii_props else MAXUNICODE
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'UnicodeData.txt'), 'r') as uf:
        for line in uf:
            data = line.strip().split(';')
            if data:
                bidi = data[4].strip().lower()
                if not bidi:
                    continue
                value = int(data[0].strip(), 16)

                if bidi not in bidi_class:
                    bidi_class[bidi] = []

                if value > max_range:
                    continue

                bidi_class[bidi].append(value)

    for name in list(bidi_class.keys()):
        s = set(bidi_class[name])
        bidi_class[name] = sorted(s)

    # Convert characters values to ranges
    char2range(bidi_class, binary=ascii_props)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        f.write('%s_bidi_classes = {\n' % prefix)
        count = len(bidi_class) - 1
        i = 0
        for k1, v1 in sorted(bidi_class.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_posix(output, binary=False, append=False, prefix=""):
    """Generate the binary posix table and write out to file."""

    posix_table = {}

    # Alnum: [a-zA-Z0-9]
    s = set([x for x in range(0x30, 0x39 + 1)])
    s |= set([x for x in range(0x41, 0x5a + 1)])
    s |= set([x for x in range(0x61, 0x7a + 1)])
    posix_table["alnum"] = list(s)

    # Alpha: [a-zA-Z]
    s = set([x for x in range(0x41, 0x5a)])
    s |= set([x for x in range(0x61, 0x7a)])
    posix_table["alpha"] = list(s)

    # ASCII: [\x00-\x7F]
    s = set([x for x in range(0, 0x7F + 1)])
    posix_table["ascii"] = list(s)

    # Blank: [ \t]
    s = set([0x20, 0x09])
    posix_table["blank"] = list(s)

    # Cntrl: [\x00-\x1F\x7F]
    s = set([x for x in range(0, 0x1F + 1)] + [0x7F])
    posix_table["cntrl"] = list(s)

    # Digit: [0-9]
    s = set([x for x in range(0x30, 0x39 + 1)])
    posix_table["digit"] = list(s)

    # Graph: [\x21-\x7E]
    s = set([x for x in range(0x21, 0x7E + 1)])
    posix_table["graph"] = list(s)

    # Lower: [a-z]
    s = set([x for x in range(0x61, 0x7a + 1)])
    posix_table["lower"] = list(s)

    # Print: [\x20-\x7E]
    s = set([x for x in range(0x20, 0x7E + 1)])
    posix_table["print"] = list(s)

    # Punct: [!\"\#$%&'()*+,\-./:;<=>?@\[\\\]^_`{|}~]
    s = set([x for x in range(0x21, 0x2f + 1)])
    s |= set([x for x in range(0x3a, 0x40 + 1)])
    s |= set([x for x in range(0x5b, 0x60 + 1)])
    s |= set([x for x in range(0x7b, 0x7e + 1)])
    posix_table["punct"] = list(s)

    # Space: [ \t\r\n\v\f]
    s = set([x for x in range(0x09, 0x0d + 1)] + [0x20])
    posix_table["space"] = list(s)

    # Upper: [A-Z]
    s = set([x for x in range(0x41, 0x5a + 1)])
    posix_table["upper"] = list(s)

    # XDigit: [A-Fa-f0-9]
    s = set([x for x in range(0x30, 0x39 + 1)])
    s |= set([x for x in range(0x41, 0x46 + 1)])
    s |= set([x for x in range(0x61, 0x66 + 1)])
    posix_table["xdigit"] = list(s)

    # Convert characters values to ranges
    char2range(posix_table, binary=binary)

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_posix_properties = {\n' % prefix)
        count = len(posix_table) - 1
        i = 0
        for k1, v1 in sorted(posix_table.items()):
            f.write('    "%s": "%s"' % (k1, v1))
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_uposix(table, posix_table):
    """Generate the posix table and write out to file."""

    # Alnum: [\p{L&}\p{Nd}]
    s = set(table['l']['c'] + table['n']['d'])
    posix_table["posixalnum"] = list(s)

    # Alpha: [\p{L&}]
    s = set(table['l']['c'])
    posix_table["posixalpha"] = list(s)

    # ASCII: [\x00-\x7F]
    s = set([x for x in range(0, 0x7F + 1)])
    posix_table["posixascii"] = list(s)

    # Blank: [\p{Zs}\t]
    s = set(table['z']['s'] + [0x09])
    posix_table["posixblank"] = list(s)

    # Cntrl: [\p{Cc}]
    s = set(table['c']['c'])
    posix_table["posixcntrl"] = list(s)

    # Digit: [\p{Nd}]
    s = set(table['n']['d'])
    posix_table["posixdigit"] = list(s)

    # Graph: [^\p{Z}\p{C}]
    s = set()
    for table_name in ('z', 'c'):
        for sub_table_name in table[table_name]:
            if not sub_table_name.startswith('^'):
                s |= set(table[table_name][sub_table_name])
    posix_table["^posixgraph"] = list(s)

    # Lower: [\p{Ll}]
    s = set(table['l']['l'])
    posix_table["posixlower"] = list(s)

    # Print: [\P{C}]
    s = set()
    for table_name in ('c',):
        for sub_table_name in table[table_name]:
            if not sub_table_name.startswith('^'):
                s |= set(table[table_name][sub_table_name])
    posix_table["^posixprint"] = list(s)

    # Punct: [\p{P}\p{S}]
    s = set()
    for table_name in ('p', 's'):
        for sub_table_name in table[table_name]:
            if not sub_table_name.startswith('^'):
                s |= set(table[table_name][sub_table_name])
    posix_table["posixpunct"] = list(s)

    # Space: [\p{Z}\t\r\n\v\f]
    s = set()
    for table_name in ('z',):
        for sub_table_name in table[table_name]:
            if not sub_table_name.startswith('^'):
                s |= set(table[table_name][sub_table_name])
    s |= set([x for x in range(0x09, 0x0e)])
    posix_table["posixspace"] = list(s)

    # Upper: [\p{Lu}]
    s = set(table['l']['u'])
    posix_table["posixupper"] = list(s)

    # XDigit: [A-Fa-f0-9]
    s = set([x for x in range(0x30, 0x39 + 1)])
    s |= set([x for x in range(0x41, 0x46 + 1)])
    s |= set([x for x in range(0x61, 0x66 + 1)])
    posix_table["posixxdigit"] = list(s)


def gen_alias(enum, binary, output, ascii_props=False, append=False, prefix=""):
    """Generate alias."""

    alias_re = re.compile(r'^#\s+(\w+)\s+\((\w+)\)\s*$')

    categories = enum + binary
    alias = {}
    gather = False
    current_category = None
    line_re = None
    alias_header_re = re.compile(r'^#\s+(\w+)\s+Properties\s*$')
    divider_re = re.compile(r'#\s*=+\s*$')
    posix_props = ('alnum', 'blank', 'graph', 'print', 'xdigit')
    toplevel = (
        'catalog', 'enumerated', 'numeric', 'miscellaneous'
    )

    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'PropertyAliases.txt'), 'r') as uf:
        div = False
        capture = False
        name = None
        for line in uf:
            if div:
                m = alias_header_re.match(line)
                if m:
                    name = format_name(m.group(1))
                    if name in toplevel:
                        capture = True
                        name = '_'
                    elif name in ('binary',):
                        capture = True
                    else:
                        capture = False
                    continue
                div = False
            elif divider_re.match(line):
                div = True
                continue
            elif line.startswith('#') or not line.strip():
                continue
            if capture:
                should_add = False
                data = [format_name(x) for x in line.split('#')[0].split(';')]
                index = 0
                for d in data:
                    if d in categories:
                        should_add = True
                        break
                    index += 1
                if should_add:
                    data[0], data[index] = data[index], data[0]
                    if name not in alias:
                        alias[name] = {}
                    for d in data[1:]:
                        alias[name][d] = data[0]

    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'PropertyValueAliases.txt'), 'r') as uf:
        for line in uf:
            m = alias_re.match(line)
            if m:
                original_name = format_name(m.group(1))
                gather = original_name in categories
                current_category = format_name(m.group(2))
                line_re = re.compile(r'%s\s*;' % m.group(2), re.I)
            if gather and line_re.match(line):
                data = [format_name(x) for x in line.split('#')[0].split(';')]
                if current_category in ('sc', 'blk', 'dt', 'jg', 'sb', 'wb', 'lb', 'gcb', 'nt', 'inpc', 'inmc', 'insc'):
                    data[1], data[2] = data[2], data[1]
                elif current_category == 'age' and UNIVERSION_INFO < (6, 1, 0):
                    if data[2] == 'unassigned':
                        data[1] = 'na'
                    else:
                        data[1], data[2] = data[2], 'V' + data[2].replace('.', '_')
                if len(data) == 5 and data[2] in ('yes', 'no') and data[1] in ('n', 'y'):
                    data = ['binary', original_name, data[0]]
                else:
                    data[0] = alias['_'].get(data[0], data[0])
                if data[0] not in alias:
                    alias[data[0]] = {}
                for a in data[2:]:
                    if a == 'n/a':
                        continue
                    if a not in alias[data[0]] and a != data[1]:
                        alias[data[0]][a] = data[1]

    for x in enum:
        if x not in alias:
            alias[x] = {}

    for prop in posix_props:
        alias['binary'][prop] = 'posix' + prop

    with codecs.open(output, 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        f.write('%s_alias = {\n' % prefix)
        count = len(alias) - 1
        i = 0
        for k1, v1 in sorted(alias.items()):
            f.write('    "%s": {\n' % k1)
            count2 = len(v1) - 1
            j = 0
            for k2, v2 in sorted(v1.items()):
                f.write('        "%s": "%s"' % (k2, v2))
                if j == count2:
                    f.write('\n    }')
                else:
                    f.write(',\n')
                j += 1
            if count2 < 0:
                f.write('    }')
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1

        cat = set(enum)
        for k in alias['_'].keys():
            cat.add(k)

        f.write('enum_names = {\n')
        count = len(cat) - 1
        i = 0
        for name in sorted(cat):
            f.write('    "%s"' % name)
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1


def gen_properties(output, ascii_props=False, append=False):
    """Generate the property table and dump it to the provided file."""

    files = {
        'gc': os.path.join(output, 'generalcategory.py'),
        'blk': os.path.join(output, 'block.py'),
        'sc': os.path.join(output, 'script.py'),
        'bc': os.path.join(output, 'bidiclass.py'),
        'binary': os.path.join(output, 'binary.py'),
        'posix': os.path.join(output, 'posix.py'),
        'age': os.path.join(output, 'age.py'),
        'ea': os.path.join(output, 'eastasianwidth.py'),
        'gcb': os.path.join(output, 'graphemeclusterbreak.py'),
        'lb': os.path.join(output, 'linebreak.py'),
        'sb': os.path.join(output, 'sentencebreak.py'),
        'wb': os.path.join(output, 'wordbreak.py'),
        'hst': os.path.join(output, 'hangulsyllabletype.py'),
        'dt': os.path.join(output, 'decompositiontype.py'),
        'jt': os.path.join(output, 'joiningtype.py'),
        'jg': os.path.join(output, 'joininggroup.py'),
        'nt': os.path.join(output, 'numerictype.py'),
        'nv': os.path.join(output, 'numericvalue.py'),
        'ccc': os.path.join(output, 'canonicalcombiningclass.py'),
        'qc': os.path.join(output, 'quickcheck.py'),
        'alias': os.path.join(output, 'alias.py')
    }

    if PY3:
        files['scx'] = os.path.join(output, 'scriptextensions.py')
        files['insc'] = os.path.join(output, 'indicsyllabiccategory.py')
        if PY34:
            files['bpt'] = os.path.join(output, 'bidipairedbrackettype.py')
        if PY35:
            files['inpc'] = os.path.join(output, 'indicpositionalcategory.py')
        else:
            files['inmc'] = os.path.join(output, 'indicmatracategory.py')
        if PY37:
            files['vo'] = os.path.join(output, 'verticalorientation.py')

    prefix = "ascii" if ascii_props else 'unicode'

    # L& or Lc won't be found in the table,
    # so intialize 'c' at the start. & will have to be converted to 'c'
    # before sending it through.
    categories = [
        'generalcategory', 'script', 'block',
        'bidiclass', 'eastasianwidth', 'linebreak',
        'hangulsyllabletype', 'wordbreak', 'sentencebreak',
        'graphemeclusterbreak', 'decompositiontype', 'joiningtype',
        'joininggroup', 'numerictype', 'numericvalue',
        'canonicalcombiningclass', 'age'
    ]
    if PY3:
        categories.append('scriptextensions')
        categories.append('indicsyllabiccategory')
        if PY34:
            categories.append('bidipairedbrackettype')
        if PY35:
            categories.append('indicpositionalcategory')
        else:
            categories.append('indicmatracategory')
        if PY37:
            categories.append('verticalorientation')
    if ascii_props:
        print('=========Ascii Tables=========')
    else:
        print('========Unicode Tables========')
    print('Building: General Category')
    max_range = ASCII_RANGE if ascii_props else UNICODE_RANGE
    all_chars = ALL_ASCII if ascii_props else ALL_CHARS
    table = {'l': {'c': []}}
    itable = {'l': {}}
    with open(os.path.join(HOME, 'unicodedata', UNIVERSION, 'UnicodeData.txt'), 'r') as uf:
        for line in uf:
            data = line.strip().split(';')
            if data:
                i = int(data[0], 16)
                if i > max_range[1]:
                    continue
                p = data[2].lower()
                if p[0] not in table:
                    table[p[0]] = {}
                    itable[p[0]] = {}
                if p[1] not in table[p[0]]:
                    table[p[0]][p[1]] = []
                table[p[0]][p[1]].append(i)
                # Add LC which is a combo of Ll, Lu, and Lt
                if p[0] == 'l' and p[1] in ('l', 'u', 't'):
                    table['l']['c'].append(i)

    # Create inverse of each category
    for k1, v1 in table.items():
        inverse_category = set()
        for k2, v2 in v1.items():
            s = set(v2)
            inverse_category |= s
        itable[k1]['^'] = list(all_chars - inverse_category)

    # Generate Unicode blocks
    print('Building: Blocks')
    gen_blocks(files['blk'], ascii_props, append, prefix)

    # Generate Unicode scripts
    print('Building: Scripts & Script Extensions')
    if PY3:
        gen_scripts(
            'Scripts.txt', 'ScriptExtensions.txt', 'scripts', 'script_extensions', files['sc'], files['scx'],
            notexplicit='zzzz', ascii_props=ascii_props, append=append, prefix=prefix
        )
    else:
        gen_scripts(
            'Scripts.txt', None, 'scripts', None, files['sc'], None,
            notexplicit='zzzz', ascii_props=ascii_props, append=append, prefix=prefix
        )

    # Generate Unicode bidi classes
    print('Building: Bidi Classes')
    gen_bidi(files['bc'], ascii_props, append, prefix)

    if PY34:
        print('Building: Bidi Paired Bracket Type')
        gen_enum(
            'BidiBrackets.txt', 'bidi_paired_bracket_type', files['bpt'], notexplicit='n',
            field=2, ascii_props=ascii_props, append=append, prefix=prefix
        )

    # Generate Unicode binary
    print('Building: Binary')
    binary = gen_binary(table, files['binary'], ascii_props, append, prefix)

    # Generate posix table and write out to file.
    print('Building: Posix')
    gen_posix(files['posix'], binary=ascii_props, append=append, prefix=prefix)

    print('Building: Age')
    gen_age(files['age'], ascii_props, append, prefix)

    print('Building: East Asian Width')
    gen_enum(
        'EastAsianWidth.txt', 'east_asian_width', files['ea'],
        notexplicit='n', ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Grapheme Cluster Break')
    gen_enum(
        'GraphemeBreakProperty.txt', 'grapheme_cluster_break', files['gcb'], notexplicit='other',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Line Break')
    gen_enum(
        'LineBreak.txt', 'line_break', files['lb'], notexplicit='unknown',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Sentence Break')
    gen_enum(
        'SentenceBreakProperty.txt', 'sentence_break', files['sb'], notexplicit='other',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Word Break')
    gen_enum(
        'WordBreakProperty.txt', 'word_break', files['wb'], notexplicit='other',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    if PY3:
        if PY35:
            print('Building: Indic Positional Category')
            gen_enum(
                'IndicPositionalCategory.txt', 'indic_positional_category', files['inpc'], notexplicit='na',
                ascii_props=ascii_props, append=append, prefix=prefix
            )
        else:
            print('Building: Indic Matra Category')
            gen_enum(
                'IndicMatraCategory.txt', 'indic_matra_category', files['inmc'], notexplicit='na',
                ascii_props=ascii_props, append=append, prefix=prefix
            )
        print('Building: Indic Syllabic Category')
        gen_enum(
            'IndicSyllabicCategory.txt', 'indic_syllabic_category', files['insc'], notexplicit='other',
            ascii_props=ascii_props, append=append, prefix=prefix
        )

    print('Building: Hangul Syllable Type')
    gen_enum(
        'HangulSyllableType.txt', 'hangul_syllable_type', files['hst'], notexplicit='na',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Decomposition Type')
    gen_enum(
        'DerivedDecompositionType.txt', 'decomposition_type', files['dt'], notexplicit='none',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Joining Type')
    gen_enum(
        'DerivedJoiningType.txt', 'joining_type', files['jt'], notexplicit='u',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Joining Group')
    gen_enum(
        'DerivedJoiningGroup.txt', 'joining_group', files['jg'], notexplicit='nonjoining',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Numeric Type')
    gen_enum(
        'DerivedNumericType.txt', 'numeric_type', files['nt'], notexplicit='none',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Numeric Value')
    gen_enum(
        'DerivedNumericValues.txt', 'numeric_values', files['nv'], field=3, notexplicit='nan',
        ascii_props=ascii_props, append=append, prefix=prefix
    )

    print('Building: Canonical Combining Class')
    gen_ccc(files['ccc'], ascii_props, append, prefix)

    print('Building: NF* Quick Check')
    categories.extend(gen_nf_quick_check(files['qc'], ascii_props, append, prefix))

    if PY37:
        print('Building: Vertical Orientation')
        gen_enum(
            'VerticalOrientation.txt', 'vertical_orientation', files['vo'], notexplicit='R',
            ascii_props=ascii_props, append=append, prefix=prefix
        )

    if not ascii_props:
        print('Building: Aliases')
        gen_alias(categories, binary, files['alias'], ascii_props, append, prefix)

    # Convert char values to string ranges.
    char2range(table, binary=ascii_props)
    char2range(itable, binary=ascii_props, invert=False)
    for k1, v1 in itable.items():
        table[k1]['^'] = v1['^']

    with codecs.open(files['gc'], 'a' if append else 'w', 'utf-8') as f:
        if not append:
            f.write(HEADER)
        # Write out the unicode properties
        f.write('%s_properties = {\n' % prefix)
        count = len(table) - 1
        i = 0
        for k1, v1 in sorted(table.items()):
            f.write('    "%s": {\n' % k1)
            count2 = len(v1) - 1
            j = 0
            for k2, v2 in sorted(v1.items()):
                f.write('        "%s": "%s"' % (k2, v2))
                if j == count2:
                    f.write('\n    }')
                else:
                    f.write(',\n')
                j += 1
            if i == count:
                f.write('\n}\n')
            else:
                f.write(',\n')
            i += 1

    if not append:
        with codecs.open(os.path.join(output, '__init__.py'), 'w') as f:
            f.write(HEADER)
            for x in sorted(files):
                f.write('from .%s import *  # noqa\n' % os.path.basename(files[x])[:-3])


def build_unicode_property_table(output):
    """Build and write out Unicode property table."""

    if not os.path.exists(output):
        os.mkdir(output)
    gen_properties(output)


def build_ascii_property_table(output):
    """Build and write out Unicode property table."""

    if not os.path.exists(output):
        os.mkdir(output)
    gen_properties(output, ascii_props=True, append=True)


def build_tables(output):
    """Build output tables."""

    build_unicode_property_table(output)
    build_ascii_property_table(output)


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(prog='unipropgen', description='Generate a unicode property table.')
    parser.add_argument('--version', action='version', version="%(prog)s " + __version__)
    parser.add_argument('output', default=None, help='Output file.')
    args = parser.parse_args()

    build_tables(args.output)
