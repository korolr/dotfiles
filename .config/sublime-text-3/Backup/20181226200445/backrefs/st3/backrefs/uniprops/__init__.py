"""UniProps."""
from __future__ import unicode_literals
from . import unidata
import sys

PY3 = sys.version_info >= (3, 0) and sys.version_info[0:2] < (4, 0)
if PY3:
    binary_type = bytes  # noqa
else:
    binary_type = str  # noqa


def get_posix_property(value, uni=False):
    """Retrieve the posix category."""

    if isinstance(value, binary_type):
        return unidata.bposix_properties[value.decode('utf-8')]
    elif uni:
        return unidata.unicode_binary[
            ('^posix' + value[1:]) if value.startswith('^') else ('posix' + value)
        ]
    else:
        return unidata.posix_properties[value]


def get_gc_property(value):
    """Get GC property."""

    if value.startswith('^'):
        negate = True
        value = value[1:]
    else:
        negate = False

    value = unidata.unicode_alias['generalcategory'].get(value, value)

    assert 1 <= len(value) <= 2, 'Invalid property!'

    if not negate:
        p1, p2 = (value[0], value[1]) if len(value) > 1 else (value[0], None)
        return ''.join(
            [v for k, v in unidata.unicode_properties.get(p1, {}).items() if not k.startswith('^')]
        ) if p2 is None else unidata.unicode_properties.get(p1, {}).get(p2, '')
    else:
        p1, p2 = (value[0], value[1]) if len(value) > 1 else (value[0], '')
        return unidata.unicode_properties.get(p1, {}).get('^' + p2, '')


def get_binary_property(value):
    """"Get BINARY property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['binary'].get(negated, negated)
    else:
        value = unidata.unicode_alias['binary'].get(value, value)

    return unidata.unicode_binary[value]


def get_canonical_combining_class_property(value):
    """"Get CANONICAL COMBINING CLASS property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['canonicalcombiningclass'].get(negated, negated)
    else:
        value = unidata.unicode_alias['canonicalcombiningclass'].get(value, value)

    return unidata.uniocde_canonical_combining_class[value]


def get_east_asian_width_property(value):
    """"Get EAST ASIAN WIDTH property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['eastasianwidth'].get(negated, negated)
    else:
        value = unidata.unicode_alias['eastasianwidth'].get(value, value)

    return unidata.unicode_east_asian_width[value]


def get_grapheme_cluster_break_property(value):
    """"Get GRAPHEME CLUSTER BREAK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['graphemeclusterbreak'].get(negated, negated)
    else:
        value = unidata.unicode_alias['graphemeclusterbreak'].get(value, value)

    return unidata.unicode_grapheme_cluster_break[value]


def get_line_break_property(value):
    """"Get LINE BREAK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['linebreak'].get(negated, negated)
    else:
        value = unidata.unicode_alias['linebreak'].get(value, value)

    return unidata.unicode_line_break[value]


def get_sentence_break_property(value):
    """"Get SENTENCE BREAK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['sentencebreak'].get(negated, negated)
    else:
        value = unidata.unicode_alias['sentencebreak'].get(value, value)

    return unidata.unicode_sentence_break[value]


def get_word_break_property(value):
    """"Get WORD BREAK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['wordbreak'].get(negated, negated)
    else:
        value = unidata.unicode_alias['wordbreak'].get(value, value)

    return unidata.unicode_word_break[value]


def get_hangul_syllable_type_property(value):
    """Get HANGUL SYLLABLE TYPE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['hangulsyllabletype'].get(negated, negated)
    else:
        value = unidata.unicode_alias['hangulsyllabletype'].get(value, value)

    return unidata.unicode_hangul_syllable_type[value]


def get_decomposition_type_property(value):
    """Get DECOMPOSITION TYPE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['decompositiontype'].get(negated, negated)
    else:
        value = unidata.unicode_alias['decompositiontype'].get(value, value)

    return unidata.unicode_decomposition_type[value]


def get_nfc_quick_check_property(value):
    """Get NFC QUICK CHECK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['nfcquickcheck'].get(negated, negated)
    else:
        value = unidata.unicode_alias['nfcquickcheck'].get(value, value)

    return unidata.unicode_nfc_quick_check[value]


def get_nfd_quick_check_property(value):
    """Get NFD QUICK CHECK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['nfdquickcheck'].get(negated, negated)
    else:
        value = unidata.unicode_alias['nfdquickcheck'].get(value, value)

    return unidata.unicode_nfd_quick_check[value]


def get_nfkc_quick_check_property(value):
    """Get NFKC QUICK CHECK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['nfkcquickcheck'].get(negated, negated)
    else:
        value = unidata.unicode_alias['nfkcquickcheck'].get(value, value)

    return unidata.unicode_nfkc_quick_check[value]


def get_nfkd_quick_check_property(value):
    """Get NFKD QUICK CHECK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['nfkdquickcheck'].get(negated, negated)
    else:
        value = unidata.unicode_alias['nfkdquickcheck'].get(value, value)

    return unidata.unicode_nfkd_quick_check[value]


def get_numeric_type_property(value):
    """Get NUMERIC TYPE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['numerictype'].get(negated, negated)
    else:
        value = unidata.unicode_alias['numerictype'].get(value, value)

    return unidata.unicode_numeric_type[value]


def get_numeric_value_property(value):
    """Get NUMERIC VALUE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['numericvalue'].get(negated, negated)
    else:
        value = unidata.unicode_alias['numericvalue'].get(value, value)

    return unidata.unicode_numeric_values[value]


def get_age_property(value):
    """Get AGE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['age'].get(negated, negated)
    else:
        value = unidata.unicode_alias['age'].get(value, value)

    return unidata.unicode_age[value]


def get_joining_type_property(value):
    """Get JOINING TYPE property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['joiningtype'].get(negated, negated)
    else:
        value = unidata.unicode_alias['joiningtype'].get(value, value)

    return unidata.unicode_joining_type[value]


def get_joining_group_property(value):
    """"Get JOINING GROUP property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['joininggroup'].get(negated, negated)
    else:
        value = unidata.unicode_alias['joininggroup'].get(value, value)

    return unidata.unicode_joining_group[value]


def get_script_property(value):
    """"Get SC property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['script'].get(negated, negated)
    else:
        value = unidata.unicode_alias['script'].get(value, value)

    return unidata.unicode_scripts[value]


def get_block_property(value):
    """"Get BLK property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['block'].get(negated, negated)
    else:
        value = unidata.unicode_alias['block'].get(value, value)

    return unidata.unicode_blocks[value]


def get_bidi_property(value):
    """"Get BC property."""

    if value.startswith('^'):
        negated = value[1:]
        value = '^' + unidata.unicode_alias['bidiclass'].get(negated, negated)
    else:
        value = unidata.unicode_alias['bidiclass'].get(value, value)

    return unidata.unicode_bidi_classes[value]


def is_enum(name):
    """Check if name is an enum (not a binary) property."""

    return name in unidata.enum_names


def get_unicode_property(value, prop=None):
    """Retrieve the unicode category from the table."""

    if prop is not None:
        prop = unidata.unicode_alias['_'].get(prop, prop)
        try:
            if prop == 'generalcategory':
                return get_gc_property(value)
            elif prop == 'script':
                return get_script_property(value)
            elif prop == 'block':
                return get_block_property(value)
            elif prop == 'binary':
                return get_binary_property(value)
            elif prop == 'bidiclass':
                return get_bidi_property(value)
            elif prop == 'age':
                return get_age_property(value)
            elif prop == 'eastasianwidth':
                return get_east_asian_width_property(value)
            elif prop == 'hangulsyllabletype':
                return get_hangul_syllable_type_property(value)
            elif prop == 'decompositiontype':
                return get_decomposition_type_property(value)
            elif prop == 'canonicalcombiningclass':
                return get_canonical_combining_class_property(value)
            elif prop == 'numerictype':
                return get_numeric_type_property(value)
            elif prop == 'numericvalue':
                return get_numeric_value_property(value)
            elif prop == 'joiningtype':
                return get_joining_type_property(value)
            elif prop == 'joininggroup':
                return get_joining_group_property(value)
            elif prop == 'graphemeclusterbreak':
                return get_grapheme_cluster_break_property(value)
            elif prop == 'linebreak':
                return get_line_break_property(value)
            elif prop == 'sentencebreak':
                return get_sentence_break_property(value)
            elif prop == 'wordbreak':
                return get_word_break_property(value)
            elif prop == 'nfcquickcheck':
                return get_nfc_quick_check_property(value)
            elif prop == 'nfdquickcheck':
                return get_nfd_quick_check_property(value)
            elif prop == 'nfkcquickcheck':
                return get_nfkc_quick_check_property(value)
            elif prop == 'nfkdquickcheck':
                return get_nfkd_quick_check_property(value)
            else:
                raise ValueError('Invalid Unicode property!')
        except Exception:
            raise ValueError('Invalid Unicode property!')

    if value.startswith('^'):
        temp = value[1:]
        negate = '^'
    else:
        temp = value
        negate = ''

    try:
        return get_gc_property(value)
    except Exception:
        pass

    if temp.startswith('is'):
        try:
            return get_script_property(negate + temp[2:])
        except Exception:
            pass
    try:
        return get_script_property(value)
    except Exception:
        pass
    if temp.startswith('in'):
        try:
            return get_block_property(negate + temp[2:])
        except Exception:
            pass
    try:
        return get_block_property(value)
    except Exception:
        pass
    try:
        return get_binary_property(value)
    except Exception:
        pass

    raise ValueError('Invalid Unicode property!')
