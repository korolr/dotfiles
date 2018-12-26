"""Download `Unicodedata` files."""
from __future__ import unicode_literals
import sys
import os
import zipfile

__version__ = '2.2.0'

PY3 = sys.version_info >= (3, 0) and sys.version_info[0:2] < (4, 0)
PY34 = sys.version_info >= (3, 4)
PY35 = sys.version_info >= (3, 5)
PY37 = sys.version_info >= (3, 7)

if PY3:
    from urllib.request import urlopen
else:
    from urllib2 import urlopen

HOME = os.path.dirname(os.path.abspath(__file__))


def zip_unicode(output, version):
    """Zip the Unicode files."""

    zipper = zipfile.ZipFile(os.path.join(output, 'unicodedata', '%s.zip' % version), 'w', zipfile.ZIP_DEFLATED)
    target = os.path.join(output, 'unicodedata', version)

    print('Zipping %s.zip...' % version)

    for root, dirs, files in os.walk(target):
        for file in files:
            if file.endswith('.txt'):
                zipper.write(os.path.join(root, file), arcname=file)


def unzip_unicode(output, version):
    """Unzip the Unicode files."""

    unzipper = zipfile.ZipFile(os.path.join(output, 'unicodedata', '%s.zip' % version))
    target = os.path.join(output, 'unicodedata', version)

    print('Unzipping %s.zip...' % version)

    os.makedirs(target)

    for f in unzipper.namelist():
        # Do I need backslash on windows? Or is it forward as well?
        unzipper.extract(f, target)


def download_unicodedata(version, output=HOME, no_zip=False):
    """Download Unicode data scripts and blocks."""
    files = [
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
    ]

    if PY3:
        files.append('ScriptExtensions.txt')
        if PY35:
            files.append('IndicPositionalCategory.txt')
        else:
            files.append('IndicMatraCategory.txt')
        files.append('IndicSyllabicCategory.txt')

    if PY34:
        files.append('BidiBrackets.txt')

    if PY37:
        files.append('VerticalOrientation.txt')

    http_url = 'http://www.unicode.org/Public/%s/ucd/' % version
    ftp_url = 'ftp://ftp.unicode.org/Public/%s/ucd/' % version

    destination = os.path.join(output, 'unicodedata', version)
    if not os.path.exists(destination):
        os.makedirs(destination)

    zip_data = not no_zip

    for f in files:
        file_location = os.path.join(destination, os.path.basename(f))
        retrieved = False
        if not os.path.exists(file_location):
            for url in (ftp_url, http_url):
                furl = url + f
                try:
                    print('Downloading: %s --> %s' % (furl, file_location))
                    response = urlopen(furl, timeout=30)
                    data = response.read()
                except Exception:
                    print('Failed: %s' % url)
                    continue
                with open(file_location, 'w') as uf:
                    uf.write(data.decode('utf-8'))
                retrieved = True
                break
            if not retrieved:
                print('Failed to acquire all needed Unicode files!')
                break
        else:
            retrieved = True
            print('Skipping: found %s' % file_location)

        if not retrieved:
            zip_data = False
            break

    if zip_data and not os.path.exists(os.path.join(output, 'unicodedata', '%s.zip' % version)):
        zip_unicode(output, version)


def get_unicodedata(version, output=HOME, no_zip=False):
    """Ensure we have Unicode data to generate Unicode tables."""

    target = os.path.join(output, 'unicodedata', version)
    zip_target = os.path.join(output, 'unicodedata', '%s.zip' % version)

    if not os.path.exists(target) and os.path.exists(zip_target):
        unzip_unicode(output, version)

    # Download missing files if any. Zip if required.
    download_unicodedata(version, output, no_zip)


if __name__ == '__main__':
    import argparse
    import unicodedata

    parser = argparse.ArgumentParser(prog='unidatadownload', description='Generate a unicode property table.')
    parser.add_argument('--version', action='version', version="%(prog)s " + __version__)
    parser.add_argument('--output', default=HOME, help='Output file.')
    parser.add_argument('--unicode-version', default=None, help='Force a specific Unicode version.')
    args = parser.parse_args()

    if args.unicode_version is None:
        version = unicodedata.unidata_version
    else:
        version = args.unicode_version

    get_unicodedata(version, args.output)
