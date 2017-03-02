"""
    service_factory.compat
    ~~~~~~~~~~~~~~~~~~~~~~

    Python compatibility workarounds.

    :copyright: (c) 2015-2016 by Artem Malyshev.
    :license: GPL3, see LICENSE for more details.
"""

__all__ = ['string_types', 'BaseHTTPRequestHandler', 'HTTPServer']


try:
    string_types = basestring
except NameError:
    string_types = str


try:
    from http.server import BaseHTTPRequestHandler, HTTPServer
except ImportError:
    from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
