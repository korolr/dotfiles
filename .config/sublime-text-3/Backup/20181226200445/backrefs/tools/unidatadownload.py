"""Download Unicodedata files."""
from __future__ import unicode_literals
import sys
import os

__version__ = '1.0.0'

PY3 = sys.version_info >= (3, 0) and sys.version_info[0:2] < (4, 0)

if PY3:
    from urllib.request import urlopen
else:
    from urllib2 import urlopen

HOME = os.path.dirname(os.path.abspath(__file__))


def download_unicodedata(version, output=HOME):
    """Download unicode data scripts and blocks."""
    files = (
        'UnicodeData.txt',
        'Scripts.txt',
        'Blocks.txt',
        'PropList.txt',
        'DerivedCoreProperties.txt',
        'DerivedNormalizationProps.txt',
        'CompositionExclusions.txt',
        'PropertyValueAliases.txt',
        'PropertyAliases.txt',
        'EastAsianWidth.txt',
        'LineBreak.txt',
        'HangulSyllableType.txt',
        'DerivedAge.txt',
        'auxiliary/WordBreakProperty.txt',
        'auxiliary/SentenceBreakProperty.txt',
        'auxiliary/GraphemeBreakProperty.txt',
        'extracted/DerivedDecompositionType.txt',
        'extracted/DerivedNumericType.txt',
        'extracted/DerivedNumericValues.txt',
        'extracted/DerivedJoiningType.txt',
        'extracted/DerivedJoiningGroup.txt',
        'extracted/DerivedCombiningClass.txt'
    )
    url = 'http://www.unicode.org/Public/%s/ucd/' % version

    destination = os.path.join(output, 'unicodedata', version)
    if not os.path.exists(destination):
        os.makedirs(destination)
    for f in files:
        furl = url + f
        file_location = os.path.join(destination, os.path.basename(f))
        if not os.path.exists(file_location):
            print('Downloading: %s --> %s' % (furl, file_location))
            response = urlopen(furl)
            data = response.read()
            with open(file_location, 'w') as uf:
                uf.write(data.decode('utf-8'))
        else:
            print('Skipping: found %s' % file_location)


if __name__ == '__main__':
    import argparse
    import unicodedata

    parser = argparse.ArgumentParser(prog='unipropgen', description='Generate a unicode property table.')
    parser.add_argument('--version', action='version', version="%(prog)s " + __version__)
    parser.add_argument('--output', default=HOME, help='Output file.')
    args = parser.parse_args()

    version = unicodedata.unidata_version
    download_unicodedata(version, args.output)
