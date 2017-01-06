# python 3
import ssl
import http.cookiejar
import sys
import urllib.request
import urllib.parse

assert len(sys.argv) == 4, 'usage: python %s <user> <pwd> <club>' % sys.argv[0]
user, pwd, club = sys.argv[1:]

opener = urllib.request.build_opener(urllib.request.HTTPCookieProcessor(http.cookiejar.CookieJar()),
                                     urllib.request.HTTPSHandler(context=ssl.SSLContext()))
opener.open('https://www.bivio.com/')
opener.open(
    'https://www.bivio.com/pub/login',
    bytes(urllib.parse.urlencode({
        'x1': user,
        'x2': pwd,
        'v': '2'
    }),encoding='utf-8'))

zip_data = opener.open('https://www.bivio.com/%s/admin/clubexp.xml.zip' % club).read()
outfile = '%s.xml.zip' % club;
file = open(outfile, 'wb')
file.write(bytearray(zip_data))
file.close()
print(outfile)
